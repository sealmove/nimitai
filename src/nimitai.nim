import json, macros, tables
import nimitai/[types, ksast, identutils]

proc needRaw(f: Field): bool =
  fkSize in f.keys or fkSizeEos in f.keys or f.`type`.kind == ktkStr

proc parseByteArray(io: NimNode; field: Field): NimNode =
  let
    term = if fkTerminator in field.keys: field.terminator else: 0
    incl = field.`include`
    cons = field.consume
    eoserr = field.eosError
  result = newCall(
    ident"readBytesTerm",
    io,
    newLit(term),
    newLit(incl),
    newLit(cons),
    newLit(eoserr))

proc parseTyped(io: NimNode; meta: Meta, id, ancestorNimId: string,
                kst: KsType): NimNode =
  let raw = ident(id & "Raw")

  case kst.kind
  of ktkBit:
    let suffix =
      if kst.bitEndian == eNone:
        $meta.bitEndian
      else:
        $kst.bitEndian
    result = newCall("readBitsInt" & suffix, io, newLit(kst.bits))

    # Bool
    if kst.bits == 1:
      result = newCall(ident"bool", result)

  of ktkUInt, ktkSInt, ktkFloat:
    let endian = if kst.endian == eNone: meta.endian else: kst.endian
    result = newCall(
      "read" & $kst.kind & $kst.bytes & (if kst.bytes != 1: $endian else: ""),
      io)

  of ktkStr:
    result = newCall(ident"toString", raw)
  #if fkEncoding in field.keys
  #if fkEosError in field.keys

  of ktkArr: discard

  of ktkUser, ktkEnum:
    result = newCall(
      ident"read",
      kst.toNim,
      io,
      newDotExpr(
        ident"this",
        ident"root"),
      ident"this")

  of ktkStream: discard #XXX

  of ktkSwitch:
    result = newTree(nnkIfStmt)
    for i in 0 ..< kst.cases.len:
      result.add nnkElifBranch.newTree(
        infix(
          kst.on.toNim,
          "==",
          kst.cases[i].v.toNim),
        nnkObjConstr.newTree(
          ident(variantId(ancestorNimId, id)),
          nnkExprColonExpr.newTree(
            ident"discr",
            newDotExpr(
              ident(variantDiscrId(ancestorNimId, id)),
              ident("case" & $i))),
          nnkExprColonExpr.newTree(
            ident("c" & $i),
            parseTyped(io, meta, id, ancestorNimId, kst.cases[i].t))))
    result.add nnkElse.newTree(
      nnkObjConstr.newTree(
        ident(variantId(ancestorNimId, id)),
        nnkExprColonExpr.newTree(
          ident"discr",
          newDotExpr(
            ident(variantDiscrId(ancestorNimId, id)),
            ident("case0"))),
        nnkExprColonExpr.newTree(
          ident("c0"),
          parseTyped(io, meta, id, ancestorNimId, kst.cases[0].t))))


proc parseField(field: Field): NimNode =
  result = newStmtList()
  var
    parseStmts = newStmtList()
    io = field.io.toNim
  let
    f = newDotExpr(
      ident"this",
      ident(
        if field.kind == fkAttr:
          field.id
        else:
          instId(field.id)))
    meta = field.st.meta
    raw = ident(field.id & "Raw")
    kst = field.`type`

  # They just add to beginning of body
  if fkDoc in field.keys:
    result.add(newCommentStmtNode(field.doc))

  if fkDocRef in field.keys:
    result.add(newCommentStmtNode(field.docRef))

  # This is not coded as a wrapper because the value of `io` changes
  if fkPos in field.keys:
    parseStmts.add(
      newLetStmt(
        ident(field.id & "SavePos"),
        newCall(ident"pos", io)),
      newCall(
        ident"seek",
        io,
        newCall(ident"int", field.pos.toNim)))

  if needRaw(field):
    var data: NimNode
    if fkSize in field.keys:
      let size = newCall(ident"int", field.size.toNim)
      data = newCall(ident"readBytes", io, size)
    else:
      data = newCall(ident"readBytesFull", io)

    if fkPadRight in field.keys and fkTerminator notin field.keys:
      data = newCall(
        ident"bytesStripRight",
        data,
        newLit(field.padRight))

    parseStmts.add(newLetStmt(raw, data))

    io = ident(field.id & "Io")
    parseStmts.add(newLetStmt(io, newCall(ident"newKaitaiStream", raw)))

  var parseExpr: NimNode
  if fkType in field.keys:
    parseExpr = parseTyped(
      io,
      field.st.meta,
      field.id,
      buildNimTypeId(field.st),
      if field.repeat == rkNone: field.`type` else: field.`type`.elemtype)
  elif fkContents in field.keys:
    parseExpr = newCall(ident"ensureFixedContents", io, newLit(field.contents))
  elif fkValue in field.keys:
    parseExpr = newCall(field.`type`.toNim, field.value.toNim)
  elif fkTerminator in field.keys:
    let term = if fkTerminator in field.keys: field.terminator else: 0
    parseExpr = parseByteArray(io, field)
  else:
    parseExpr = ident(field.id & "Raw")

  # They wrap parseExpr (careful, order matters)
  # if fkProcess in field.keys: XXX
  if fkEnum in field.keys and fkValue notin field.keys:
    parseExpr = newCall(
      ident(matchAndBuildEnum(field.`enum`, field.st)), parseExpr)
  # if fkProcess

  # They define extra structuring around the parsing core
  case field.repeat
  of rkNone:
    parseStmts.add(newAssignment(f, parseExpr))
  of rkEos:
    parseStmts.add(
      nnkWhileStmt.newTree(
        prefix(newCall(ident"eof", io), "not"),
        newCall(
          ident"add",
          f,
          parseExpr)))
  of rkExpr:
    if fkRepeatExpr notin field.keys:
      raise newException(
        KaitaiError,
        "'repeat' kind is 'expr' but no 'repeat-expr' key found")
    parseStmts =
      newBlockStmt(
        newEmptyNode(),
        newStmtList(
          newLetStmt(
            ident"expr",
            field.repeatExpr.toNim),
          newCall(ident"setlen", f, ident"expr"),
          nnkForStmt.newTree(
            ident"index",
            infix(
              newLit(0),
              "..<",
              ident"expr"),
            newStmtList(
              parseStmts,
              newAssignment(
                nnkBracketExpr.newTree(f, ident"index"),
                parseExpr)))))
  of rkUntil:
    if fkRepeatUntil notin field.keys:
      raise newException(
        KaitaiError,
        "'repeat' kind is 'until' but no 'repeat-until' key found")
    parseStmts =
      newBlockStmt(
        newEmptyNode(),
        newStmtList(
          nnkVarSection.newTree(
            newIdentDefs(
              ident"x",
              kst.toNim)),
          nnkWhileStmt.newTree(
            ident"true",
            newStmtList(
              parseStmts,
              newAssignment(ident"x", parseExpr),
              newCall(ident"add", f, ident"x"),
              newIfStmt((
                field.repeatUntil.toNim,
                nnkBreakStmt.newTree(newEmptyNode())))))))

  # This is not coded as a wrapper because the value of `io` changes
  if fkPos in field.keys:
    parseStmts.add(
      newCall(
        ident"seek",
        io,
        newCall(
          ident"int",
          ident(field.id & "SavePos"))))

  # It wraps all statements; this is handled seperately for instances because
  # they always have an if for caching
  if field.kind == fkAttr and fkIf in field.keys:
    parseStmts.add(
      newAssignment(
        newDotExpr(
          ident"this",
          ident(isParsedId(field.id))),
        ident"true"))
    parseStmts = newIfStmt((field.`if`.toNim, parseStmts))

  result.add(parseStmts)

proc parentType(typ: Type): NimNode =
  if typ.supertypes.len != 1:
    return ident(rootTypeName)
  return ident(buildNimTypeId(typ.supertypes[0]))

proc typeDecl(section: var NimNode, typ: Type) =
  var fields = newTree(nnkRecList)
  let id = buildNimTypeId(typ)

  fields.add(
    newIdentDefs(
      ident"parent",
      parentType(typ)))

  for a in typ.seq:
    if a.`type`.kind == ktkSwitch:
      let vdid = ident(variantDiscrId(id, a.id))

      # declare enum for the discrimination
      var enumty = nnkEnumTy.newTree(newEmptyNode())
      for i in 0 ..< a.`type`.cases.len:
        enumty.add ident("case" & $i)

      section.add nnkTypeDef.newTree(
        nnkPragmaExpr.newTree(
          vdid,
          nnkPragma.newTree(ident"pure")),
        newEmptyNode(),
        enumty)

      # declare a type for the field to encapsulate variants
      var rec = nnkRecCase.newTree(newIdentDefs(ident"discr", vdid))
      for i in 0 ..< a.`type`.cases.len:
        rec.add nnkOfBranch.newTree(
          newDotExpr(vdid, ident("case" & $i)),
          newIdentDefs(
            ident("c" & $i),
            a.`type`.cases[i].t.toNim))

      section.add nnkTypeDef.newTree(
        ident(variantId(id, a.id)),
        newEmptyNode(),
        nnkRefTy.newTree(
          nnkObjectTy.newTree(
            newEmptyNode(),
            newEmptyNode(),
            nnkRecList.newTree(rec))))

    var t =
      if a.`type`.kind == ktkSwitch:
        ident(variantId(id, a.id))
      elif fkEnum in a.keys:
        ident(matchAndBuildEnum(a.`enum`, typ))
      else:
        a.`type`.toNim
    fields.add newIdentDefs(ident(a.id), t)

    if fkIf in a.keys:
      fields.add newIdentDefs(ident(isParsedId(a.id)), ident"bool")

  for i in typ.instances:
    var t = i.`type`.toNim
    fields.add newIdentDefs(
        ident(instId(i.id)),
        t),
      newIdentDefs(
        ident(isParsedId(i.id)),
        ident"bool")

  section.add nnkTypeDef.newTree(
    ident(id),
    newEmptyNode(),
    nnkRefTy.newTree(
      nnkObjectTy.newTree(
        newEmptyNode(),
        nnkOfInherit.newTree(
          ident(rootTypeName)),
        fields)))

  var defs: seq[NimNode]
  for name, consts in typ.enums:
    var fields = nnkEnumTy.newTree(newEmptyNode())
    for n, ve in consts:
      fields.add(
        nnkEnumFieldDef.newTree(
          ident(ve.id),
          newIntLitNode(n)))
    defs.add(
      nnkTypeDef.newTree(
        ident(buildNimTypeId(typ) & name),
        newEmptyNode(),
        fields))

  for e in defs:
    section.add e

  for t in typ.types:
    typeDecl(section, t)

proc readProcParams(typ: Type): NimNode =
  let id = buildNimTypeId(typ)

  result = nnkFormalParams.newTree(
    ident(id),
    newIdentDefs(
      ident"_",
      nnkBracketExpr.newTree(
        ident"typedesc",
        ident(id))),
    newIdentDefs(
      ident"io",
      ident(streamTypeName)),
    newIdentDefs(
      ident"root",
      ident(rootTypeName)),
    newIdentDefs(
      ident"parent",
      parentType(typ)))

proc readProcFw(typ: Type): NimNode =
  result = nnkProcDef.newTree(
    ident"read",
    newEmptyNode(),
    newEmptyNode(),
    readProcParams(typ),
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode())

proc readProc(typ: Type): NimNode =
  result = newProc(name = ident"read")
  result.params = readProcParams(typ)

  let id = buildNimTypeId(typ)

  var constructor = nnkObjConstr.newTree(
    ident(id),
    newColonExpr(
      ident"io",
      ident"io"),
    newColonExpr(
      ident"parent",
      ident"parent"))

  result.body = newStmtList(
    nnkTemplateDef.newTree(
      ident"this",
      newEmptyNode(),
      newEmptyNode(),
      nnkFormalParams.newTree(ident"untyped"),
      newEmptyNode(),
      newEmptyNode(),
      ident"result"),
    newAssignment(
      ident"this",
      constructor),
    newAssignment(
      newDotExpr(
        ident"this",
        ident"root"),
      nnkIfExpr.newTree(
        nnkElifExpr.newTree(
          infix(
            ident"root",
            "==",
            newNilLit()),
          ident"this"),
        nnkElseExpr.newTree(
          ident"root"))))

  for i in 0 ..< typ.seq.len:
    # Check if we are transitioning from a bit attribute to a regular one
    if i != 0 and
       typ.seq[i-1].`type`.kind == ktkBit and
       typ.seq[i].`type`.kind != ktkBit:
      # This might be a bug when switching between ktkSwitch and non-ktkSwitch
      # field; need to check this senario # XXX
      result.body.add(
        newCall(
          ident"alignToByte",
          newDotExpr(ident"this", ident"io")))
    let stmts = parseField(typ.seq[i])
    for s in stmts: result.body.add(s)

proc instProcParams(inst: Field, typ: Type): NimNode =
  let id = buildNimTypeId(typ)
  var t = inst.`type`.toNim

  result = nnkFormalParams.newTree(
    t,
    newIdentDefs(
      ident"this",
      ident(id)))

proc instProcFw(inst: Field, typ: Type): NimNode =
  result = nnkProcDef.newTree(
    ident(inst.id),
    newEmptyNode(),
    newEmptyNode(),
    instProcParams(inst, typ),
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode())

proc instProc(inst: Field, typ: Type): NimNode =
  result = newProc(ident(inst.id))
  result.params = instProcParams(inst, typ)

  var condition = prefix(
    newDotExpr(
      ident"this",
      ident(isParsedId(inst.id))),
    "not")

  if fkIf in inst.keys:
    condition = infix(
      condition,
      "and",
      inst.`if`.toNim)

  var pa = newStmtList()
  for s in parseField(inst): pa.add(s)

  pa.add(
    newAssignment(
      newDotExpr(
        ident"this",
        ident(isParsedId(inst.id))),
      ident"true"))

  result.body = newStmtList(
    newIfStmt((condition, pa)),
    nnkReturnStmt.newTree(
      newDotExpr(
        ident"this",
        ident(instId(inst.id)))))

proc procFwDecl(stmtList: var NimNode, typ: Type) =
  for c in typ.types:
    procFwDecl(stmtList, c)
  stmtList.add(readProcFw(typ))
  for i in typ.instances:
    stmtList.add(instProcFw(i, typ))

proc procDecl(stmtList: var NimNode, typ: Type) =
  for c in typ.types:
    procDecl(stmtList, c)
  stmtList.add(readProc(typ))
  for i in typ.instances:
    stmtList.add(instProc(i, typ))

proc fromFileProc(typ: Type): NimNode =
  let id = buildNimTypeId(typ)

  newProc(
    name = ident"fromFile",
    params = @[
      ident(id),
      newIdentDefs(
        ident"_",
        nnkBracketExpr.newTree(
          ident"typedesc",
          ident(id))),
      newIdentDefs(
        ident"filename",
        ident"string")],
    body = newCall(
      ident"read",
      ident(id),
      newCall(
        ident"newKaitaiFileStream",
        ident"filename"),
        newNilLit(),
        newNilLit()))

proc fromFileProcs(typ: Type): NimNode =
  result = newStmtList()
  if typ.types != @[]:
    for c in typ.types:
      result.add(fromFileProcs(c))
  result.add(fromFileProc(typ))

proc generateParser*(spec: JsonNode): NimNode =
  let spec = spec.toType

  var
    typeSection = newTree(nnkTypeSection)
    procFwDecls = newStmtList()
    procDecls = newStmtList()

  typeDecl(typeSection, spec)
  procFwDecl(procFwDecls, spec)
  procDecl(procDecls, spec)

  result = newStmtList(
    typeSection,
    procFwDecls,
    procDecls,
    fromFileProcs(spec))

  echo repr result

# static library
macro injectParser*(spec: static[JsonNode]) =
  generateParser(spec)

# dynamic library
proc createDynlib*(spec: JsonNode, path: string) = discard

# source code
proc outputModule*(spec: JsonNode): string = discard

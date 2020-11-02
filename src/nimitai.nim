import json, macros, tables
import nimitai/[types, ksast]

const
  rootTypeName = "KaitaiStruct"
  streamTypeName = "KaitaiStream"

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

proc parseTyped(io: NimNode; field: Field): NimNode =
  let
    meta = field.st.meta
    raw = ident(field.id & "Raw")
    kst = field.`type`
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
    if fkTerminator in field.keys or kst.isZeroTerm:
      result = parseByteArray(io, field)
    else:
      result = raw
    result = newCall(ident"toString", result)

  #if fkEncoding in field.keys
  #if fkEosError in field.keys

  of ktkArr: discard #XXX
  of ktkUser, ktkEnum:
    result = newCall(
      ident"read",
      kst.toNim,
      io,
      newDotExpr(
        ident"this",
        ident"root"),
      ident"this")

proc parseExpr(io: NimNode; field: Field): NimNode =
  if fkType in field.keys:
    result = parseTyped(io, field)
  elif fkContents in field.keys:
    result = newCall(ident"ensureFixedContents", io, newLit(field.contents))
  elif fkValue in field.keys:
    result = newCall(field.`type`.toNim, field.value.toNim)
  elif fkTerminator in field.keys:
    let term = if fkTerminator in field.keys: field.terminator else: 0
    result = parseByteArray(io, field)
  else:
    result = ident(field.id & "Raw")

  # They wrap parseExpr (careful, order matters)
  # if fkProcess in field.keys: XXX
  if fkEnum in field.keys:
    result = newCall(
      ident(matchAndBuildEnum(field.`enum`, field.st)),
      result)

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
          field.id & "Inst"))
    meta = field.st.meta
    raw = ident(field.id & "Raw")
    kst = field.`type`

  # They just add to beginning of body
  if fkDoc in field.keys:
    result.add(newCommentStmtNode(field.doc))

  if fkDocRef in field.keys:
    result.add(newCommentStmtNode(field.docRef))

  if fkSize in field.keys or fkSizeEos in field.keys:
    let size = newCall(ident"int", field.size.toNim)
    var data =
      if fkSize in field.keys:
        newCall(ident"readBytes", io, size)
      else:
        newCall(ident"readBytesFull", io)

    if fkPadRight in field.keys and fkTerminator notin field.keys:
      data = newCall(
        ident"bytesStripRight",
        data,
        newLit(field.padRight))

    parseStmts.add(newLetStmt(raw, data))

    io = ident(field.id & "Io")
    parseStmts.add(newLetStmt(io, newCall(ident"newKaitaiStream", raw)))
        
  # They define extra structuring around the parsing core
  case field.repeat
  of rkNone:
    parseStmts.add(newAssignment(f, parseExpr(io, field)))
  of rkEos:
    parseStmts.add(
      nnkWhileStmt.newTree(
        prefix(newCall(ident"eof", io), "not"),
        newCall(
          ident"add",
          f,
          parseExpr(io, field))))
  of rkExpr:
    if fkRepeatExpr notin field.keys:
      raise newException(
        KaitaiError,
        "'repeat' kind is 'expr' but no 'repeat-expr' key found")
    parseStmts.add(
      newLetStmt(
        ident"expr",
        field.repeatExpr.toNim),
      newCall(ident"setlen", f, ident"expr"),
      nnkForStmt.newTree(
        ident"i",
        infix(
          newLit(0),
          "..<",
          ident"expr"),
        newAssignment(
          nnkBracketExpr.newTree(f, ident"i"),
          parseExpr(io, field))))
  of rkUntil:
    if fkRepeatUntil notin field.keys:
      raise newException(
        KaitaiError,
        "'repeat' kind is 'until' but no 'repeat-expr' key found")
    parseStmts.add(
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
              newAssignment(ident"x", parseExpr(io, field)),
              newCall(ident"add", f, ident"x"),
              newIfStmt((field.repeatUntil.toNim, newTree(nnkBreakStmt))))))))

  # It wraps all statements
  if fkPos in field.keys:
    parseStmts = newStmtList(
      newLetStmt(
        ident(field.id & "SavePos"),
        newCall(ident"pos", io)),
      newCall(
        ident"seek",
        io,
        newCall(ident"int", field.pos.toNim)),
      parseStmts,
      newCall(
        ident"seek",
        io,
        newCall(
          ident"int",
          ident(field.id & "SavePos"))))

  # It wraps all statements
  if fkIf in field.keys:
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
    var t =
      if fkEnum in a.keys:
        ident(matchAndBuildEnum(a.`enum`, typ))
      else:
        a.`type`.toNim
    if fkRepeat in a.keys:
      t = nnkBracketExpr.newTree(ident"seq", t)
    fields.add(
      newIdentDefs(
        ident(a.id),
        t))

  for i in typ.instances:
    fields.add(
      newIdentDefs(
        ident(i.id & "Inst"),
        i.`type`.toNim),
      newIdentDefs(
        ident(i.id & "Cached"),
        ident"bool"))

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

  var
    constructor = nnkObjConstr.newTree(
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
    if i != 0:
      if typ.seq[i-1].`type`.kind == ktkBit and typ.seq[i].`type`.kind != ktkBit:
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
    inst.`type`.toNim,
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

  var pa = newStmtList()
  for s in parseField(inst): pa.add(s)

  pa.add(
    newAssignment(
      newDotExpr(
        ident"this",
        ident(inst.id & "Cached")),
      ident"true"))

  result.body = newStmtList(
    newIfStmt(
     (prefix(
        newDotExpr(
          ident"this",
          ident(inst.id & "Cached")),
        "not"),
      pa)),
    nnkReturnStmt.newTree(
      newDotExpr(
        ident"this",
        ident(inst.id & "Inst"))))

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

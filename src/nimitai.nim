import json, macros, tables
import nimitai/ksast

# XXX add --> template this: untyped = result

const
  rootTypeName = "KaitaiStruct"
  streamTypeName = "KaitaiStream"

proc parentType(typ: Type): NimNode =
  if typ.parent == nil: ident(rootTypeName)
  else: ident(buildNimTypeId(typ.parent))

proc parse(field: Field, typ: Type): NimNode =
  if fkValue in field.keys:
    let t = field.`type`.ksToNimType(typ)
    if t.kind == nnkBracketExpr:
      return field.value.toNim
    else:
      return newCall(
        field.`type`.ksToNimType(typ),
        field.value.toNim)

  let t = field.`type`
  case t.kind
  of ktkBit:
    let suffix =
      if t.bitEndian == eNone:
        $typ.meta.bitEndian
      else:
        $t.bitEndian
    result = newCall(
      "readBitsInt" & suffix,
      field.io.toNim,
      newLit(t.bits))
    # Bool
    if t.bits == 1:
      result = newCall(ident"bool", result)
  of ktkUInt, ktkSInt, ktkFloat:
    let endian = if t.endian == eNone: typ.meta.endian else: t.endian
    result = newCall(
      "read" & $t.kind & $t.bytes & (if t.bytes != 1: $endian else: ""),
      field.io.toNim)
  of ktkArr: discard # XXX
  of ktkBArr, ktkStr:
    if fkTerminator in field.keys or (t.kind == ktkStr and t.isZeroTerm):
      let term = if fkTerminator in field.keys: field.terminator else: 0
      result = newCall(
        ident"readBytesTerm",
        field.io.toNim,
        newLit(term),
        newLit(field.`include`),
        newLit(field.consume),
        newLit(field.eosError))
    elif fkPadRight in field.keys:
      result = newCall(
        ident"bytesStripRight",
        newCall(
          ident"readBytes",
          field.io.toNim,
          newCall(
            ident"int",
            field.size.toNim)),
        newLit(field.padRight))
    elif field.sizeEos:
      result = newCall(
        ident"readBytesFull",
        field.io.toNim)
    else:
      result = newCall(
        ident"readBytes",
        field.io.toNim,
        newCall(
          ident"int",
          field.size.toNim))
    if t.kind == ktkStr:
      let enc = if fkEncoding in field.keys: field.encoding
                else: "UTF-8"
      result = newCall(
        ident"encode",
        result,
        newStrLitNode(enc))
  of ktkUser:
    result = newCall(
      newDotExpr(
        field.`type`.ksToNimType(typ),
        ident"read"),
      field.io.toNim,
      newDotExpr(
        ident"this",
        ident"root"),
      ident"this")

  if fkEnum in field.keys:
    result = newCall(
      ident(matchAndBuildEnum(field.`enum`, typ)),
      result)


proc substream(id, ps, ss, size: NimNode): NimNode =
  result = newStmtList()
  result.add(
    newLetStmt(
      id,
      newCall(
        ident"readBytes",
        ps,
        newCall(
          ident"int",
          size))))
  result.add(
    newLetStmt(
      ss,
      newCall(
        ident"newKaitaiStream",
        id)))

proc parseField(field: Field, typ: Type, postfix = ""): seq[NimNode] =
  let id = ident(field.id & postfix)
  var posId: NimNode

  if fkPos in field.keys:
    posId = ident(field.id & "Pos")
    result.add(
      newLetStmt(
        posId,
        newCall(
          ident"pos",
          field.io.toNim)))
    result.add(
      newCall(
        newDotExpr(
          field.io.toNim,
          ident"skip"),
        field.pos.toNim))

  if fkSize in field.keys:
    let stmts = substream(
      ident(field.id & "Raw"),
      newDotExpr(
        ident"this",
        ident"io"),
      ident(field.id & "Io"),
      field.size.toNim)
    for s in stmts: result.add(s)

  case field.repeat
  of rkNone:
    result.add(
      newLetStmt(
        id,
        parse(field, typ)))
  of rkEos:
    result.add(
      nnkWhileStmt.newTree(
        prefix(
          newCall(
            ident"eof",
            field.io.toNim),
          "not"),
        newCall(
          newDotExpr(id, ident"add"),
          parse(field, typ))))
  of rkExpr:
    discard
  of rkUntil:
    discard

  if fkPos in field.keys:
    result.add(
      newCall(
        newDotExpr(
          field.io.toNim,
          ident"seek"),
        posId))

  result.add(newAssignment(newDotExpr(ident"this", id), id))

proc typeDecl(section: var NimNode, typ: Type) =
  var fields = newTree(nnkRecList)
  let
    id = buildNimTypeId(typ)
    pt = if typ.isImpure: ident(rootTypeName)
         else: parentType(typ)

  fields.add(
    newIdentDefs(
      ident"parent",
      pt))

  for a in typ.seq:
    let t =
      if fkEnum in a.keys:
        ident(matchAndBuildEnum(a.`enum`, typ))
      else:
        a.`type`.ksToNimType(typ)
    fields.add(
      newIdentDefs(
        ident(a.id),
        t))

  for i in typ.instances:
    fields.add(
      newIdentDefs(
        ident(i.id & "Inst"),
        i.`type`.ksToNimType(typ)),
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
  let
    id = buildNimTypeId(typ)
    pt = if typ.isImpure: ident(rootTypeName)
         else: parentType(typ)

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
      pt))

proc instProcParams(inst: Field, typ: Type): NimNode =
  let id = buildNimTypeId(typ)

  result = nnkFormalParams.newTree(
    inst.`type`.ksToNimType(typ),
    newIdentDefs(
      ident"this",
      ident(id)))

proc readProcFw(typ: Type): NimNode =
  result = nnkProcDef.newTree(
    ident"read",
    newEmptyNode(),
    newEmptyNode(),
    readProcParams(typ),
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode())

proc instProcFw(inst: Field, typ: Type): NimNode =
  result = nnkProcDef.newTree(
    ident(inst.id),
    newEmptyNode(),
    newEmptyNode(),
    instProcParams(inst, typ),
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode())

proc procFwDecl(stmtList: var NimNode, typ: Type) =
  for c in typ.types:
    procFwDecl(stmtList, c)
  stmtList.add(readProcFw(typ))
  for i in typ.instances:
    stmtList.add(instProcFw(i, typ))

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
    let stmts = parseField(typ.seq[i], typ)
    for s in stmts: result.body.add(s)

proc instProc(inst: Field, typ: Type): NimNode =
  result = newProc(ident(inst.id))
  result.params = instProcParams(inst, typ)

  var templates = newStmtList()
  for a in typ.seq:
    templates.add(
      nnkTemplateDef.newTree(
        ident(a.id),
        newEmptyNode(),
        newEmptyNode(),
        nnkFormalParams.newTree(ident"untyped"),
        newEmptyNode(),
        newEmptyNode(),
        newDotExpr(
          ident"this",
          ident(a.id))))

  for i in typ.instances:
    templates.add(
      nnkTemplateDef.newTree(
        ident(i.id),
        newEmptyNode(),
        newEmptyNode(),
        nnkFormalParams.newTree(ident"untyped"),
        newEmptyNode(),
        newEmptyNode(),
        newDotExpr(
          ident"this",
          ident(i.id))))

  var pa = newStmtList()
  for s in parseField(inst, typ, postfix = "Inst"): pa.add(s)

  pa.add(
    newAssignment(
      newDotExpr(
        ident"this",
        ident(inst.id & "Cached")),
      ident"true"))

  result.body = newStmtList(
    templates,
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

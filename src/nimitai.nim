import nimitai/ksast
import json, macros, regex, strutils

const
  rootTypeName = "KaitaiStruct"
  streamTypeName = "KaitaiStream"

proc parse(field: Field, endian: EndianKind): NimNode =
  if FieldKey.value in field.keys:
    return newCall(
      field.`type`.parsed.strVal,
      field.value)

  if FieldKey.`type` in field.keys:
    let t = field.`type`.raw
    # Number
    if t.match(re"([us][1248]|f[48])(be|le)?"):
      var procName = "read" & t
      if not t.match(re"([us][1])|(.*(be|le))"):
        procName &= $endian
      result = newCall(procName, field.io)

    # Bool
    elif t == "b1":
      result = newCall(
        ident"bool",
        newCall(
          "readBitsIntBe",
          field.io,
          newLit(1)))

    # Number from bits
    elif t.match(re"b[2-9]|b[1-9][0-9]*(be|le)?"):
      let bits = parseInt(t[1..^1])
      result = newCall(
        "readBitsIntBe",
        field.io,
        newLit(bits))

    # User-defined type
    else:
      result = newCall(
        newDotExpr(
          ident(t.capitalizeAscii),
          ident"read"),
        field.io,
        newDotExpr(
          ident"result",
          ident"root"),
        ident"result")

  # Typeless
  else:
    result = newCall(
      ident"readBytes",
      field.io,
      newCall(
        ident"int",
        field.size))

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

proc parseField(field: Field, context: NimNode, endian: EndianKind,
                postfix = ""): NimNode =
  result = newStmtList()

  let id = newDotExpr(context, ident(field.id & postfix))
  var posId: NimNode

  if FieldKey.pos in field.keys:
    posId = ident(field.id & "Pos")
    result.add(
      newLetStmt(
        posId,
        newCall(
          ident"pos",
          field.io)))
    result.add(
      newCall(
        newDotExpr(
          field.io,
          ident"skip"),
        field.pos))

  if FieldKey.`size` in field.keys:
    let stmts = substream(
      ident(field.id & "Raw"),
      newDotExpr(
        context,
        ident"io"),
      ident(field.id & "Io"),
      field.size)
    for s in stmts: result.add(s)

  case field.repeat
  of none:
    result.add(
      newAssignment(
        id,
        parse(field, endian)))
  of eos:
    result.add(
      nnkWhileStmt.newTree(
        prefix(
          newCall(
            ident"eof",
            field.io),
          "not"),
        newCall(
          newDotExpr(id, ident"add"),
          parse(field, endian))))
  of expr:
    discard
  of until:
    discard

  if FieldKey.pos in field.keys:
    result.add(
      newCall(
        newDotExpr(
          field.io,
          ident"seek"),
        posId))

proc parentType(node: Type): string =
  if node.supertype == nil: rootTypeName else: node.supertype.id

proc typeDecl(section: var NimNode, node: Type) =
  var fields = newTree(nnkRecList)

  fields.add(
    newIdentDefs(
      ident"parent",
      ident(parentType(node))))

  for a in node.seq:
    fields.add(
      newIdentDefs(
        ident(a.id),
        a.`type`.parsed))

  for i in node.instances:
    fields.add(
      newIdentDefs(
        ident(i.id & "Inst"),
        i.`type`.parsed),
      newIdentDefs(
        ident(i.id & "Cached"),
        ident"bool"))

  section.add nnkTypeDef.newTree(
    ident(node.id),
    newEmptyNode(),
    nnkRefTy.newTree(
      nnkObjectTy.newTree(
        newEmptyNode(),
        nnkOfInherit.newTree(
          ident(rootTypeName)),
        fields)))

  for t in node.types:
    typeDecl(section, t)

proc readProcParams(node: Type): NimNode =
  result = nnkFormalParams.newTree(
    ident(node.id),
    newIdentDefs(
      ident"_",
      nnkBracketExpr.newTree(
        ident"typedesc",
        ident(node.id))),
    newIdentDefs(
      ident"io",
      ident(streamTypeName)),
    newIdentDefs(
      ident"root",
      ident(rootTypeName)),
    newIdentDefs(
      ident"parent",
      ident(parentType(node))))

proc instProcParams(inst: Field, node: Type): NimNode =
  result = nnkFormalParams.newTree(
    inst.`type`.parsed,
    newIdentDefs(
      ident"this",
      ident(node.id)))

proc readProcFw(node: Type): NimNode =
  result = nnkProcDef.newTree(
    ident"read",
    newEmptyNode(),
    newEmptyNode(),
    readProcParams(node),
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode())

proc instProcFw(inst: Field, node: Type): NimNode =
  result = nnkProcDef.newTree(
    ident(inst.id),
    newEmptyNode(),
    newEmptyNode(),
    instProcParams(inst, node),
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode())

proc procFwDecl(stmtList: var NimNode, node: Type) =
  for c in node.types:
    procFwDecl(stmtList, c)
  stmtList.add(readProcFw(node))
  for i in node.instances:
    stmtList.add(instProcFw(i, node))

proc readProc(node: Type): NimNode =
  result = newProc(name = ident"read")
  result.params = readProcParams(node)

  var
    constructor = nnkObjConstr.newTree(
      ident(node.id),
      newColonExpr(
        ident"io",
        ident"io"),
      newColonExpr(
        ident"parent",
        ident"parent"))

  result.body = newStmtList(
    newAssignment(
      ident"result",
      constructor),
    newAssignment(
      newDotExpr(
        ident"result",
        ident"root"),
      nnkIfExpr.newTree(
        nnkElifExpr.newTree(
          infix(
            ident"root",
            "==",
            newNilLit()),
          ident"result"),
        nnkElseExpr.newTree(
          ident"root"))))

  for i in 0 ..< node.seq.len:
    # Check if we are transitioning from a bit attribute to a regular one
    if i != 0:
      let
        r = re"b[1-9][0-9]*(be|le)?"
        prevType = node.seq[i-1].`type`.raw
        currType = node.seq[i].`type`.raw
      if prevType.match(r) and not currType.match(r):
        result.body.add(
          newCall(
            ident"alignToByte",
            newDotExpr(ident"result", ident"io")))
    let stmts = parseField(node.seq[i], ident"result", node.meta.endian)
    for s in stmts: result.body.add(s)

proc instProc(inst: Field, node: Type): NimNode =
  result = newProc(ident(inst.id))
  result.params = instProcParams(inst, node)

  var pa = parseField(inst, ident"this", node.meta.endian, postfix = "Inst")
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

proc procDecl(stmtList: var NimNode, node: Type) =
  for c in node.types:
    procDecl(stmtList, c)
  stmtList.add(readProc(node))
  for i in node.instances:
    stmtList.add(instProc(i, node))

proc fromFileProc(node: Type): NimNode =
  newProc(
    name = ident"fromFile",
    params = @[
      ident(node.id),
      newIdentDefs(
        ident"_",
        nnkBracketExpr.newTree(
          ident"typedesc",
          ident(node.id))),
      newIdentDefs(
        ident"filename",
        ident"string")],
    body = newCall(
      ident"read",
      ident(node.id),
      newCall(
        ident"newKaitaiFileStream",
        ident"filename"),
        newNilLit(),
        newNilLit()))

proc fromFileProcs(node: Type): NimNode =
  result = newStmtList()
  if node.types != @[]:
    for c in node.types:
      result.add(fromFileProcs(c))
  result.add(fromFileProc(node))

proc generateParser*(spec: JsonNode): NimNode =
  let spec = spec.toKsType

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

  echo treerepr result
  echo repr result

# static library
macro injectParser*(spec: static[JsonNode]) =
  generateParser(spec)

# dynamic library
proc createDynlib*(spec: JsonNode, path: string) = discard

# source code
proc outputModule*(spec: JsonNode): string = discard

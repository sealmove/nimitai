import nimitai/ksast
import json, macros, regex, strutils

const
  rootTypeName = "KaitaiStruct"
  streamTypeName = "KaitaiStream"

# A series of assignments of parsing calls to local variables or object fields
proc parseAttr(attr: Attr, endian: EndianKind): NimNode =
  result = newStmtList()

  # This should be used for both size and sizeless attributes
  var stream, size: NimNode

  # Size key means we get a substream
  if AttrKey.`size` in attr.set:
    stream = ident(attr.id & "Io")
    let raw = ident(attr.id & "Raw")
    result.add(
      newLetStmt(
        raw,
        newCall(
          newDotExpr(
            newDotExpr(
              ident"result",
              ident"io"),
            ident"readBytes"),
          newCall(
            ident"int",
            attr.size))))
    result.add(
      newLetStmt(
        stream,
        newCall(
          ident"newKaitaiStream",
          raw)))

  if AttrKey.`type` in attr.set:
    let t = attr.`type`.raw
    # Number
    if t.match(re"([us][1248]|f[48])(be|le)?"):
      var procName = "read" & t
      if not t.match(re"([us][1])|(.*(be|le))"):
        procName &= $endian
      result.add(
        newAssignment(
          newDotExpr(
            ident"result",
            ident(attr.id)),
            newCall(
              procName,
              newDotExpr(
                ident"result",
                ident"io"))))

    # Bool
    elif t == "b1":
      result.add(
        newAssignment(
          newDotExpr(
            ident"result",
            ident(attr.id)),
          newCall(
            ident"bool",
            newCall(
              "readBitsIntBe",
              newDotExpr(
                ident"result",
                ident"io"),
              newLit(1)))))

    # Number from bits
    elif t.match(re"b[2-9]|b[1-9][0-9]*(be|le)?"):
      let bits = parseInt(t[1..^1])
      result.add(
        newAssignment(
          newDotExpr(
            ident"result",
            ident(attr.id)),
          newCall(
            "readBitsIntBe",
            newDotExpr(
              ident"result",
              ident"io"),
            newLit(bits))))

    # User-defined type
    else:
      result.add(
        newAssignment(
          newDotExpr(
            ident"result",
            ident(attr.id)),
          newCall(
            newDotExpr(
              ident(t.capitalizeAscii),
              ident"read"),
            stream,
            newDotExpr(
              ident"result",
              ident"root"),
            ident"result")))

  # Typeless
  else:
    result.add(
      newAssignment(
        newDotExpr(
          ident"result",
          ident(attr.id)),
        newDotExpr(
          stream,
          newCall(
            ident"readBytes",
            attr.size))))

# Only value-instances supported for now
proc instanceProc(inst: Attr, node: Type): NimNode =
  result = newProc(
    ident(inst.id),
    @[inst.`type`.parsed,
      newIdentDefs(
        ident"this",
        ident(node.id))])
  result.body = newStmtList(
    newIfStmt(
     (prefix(
        newDotExpr(
          ident"this",
          ident(inst.id & "Cached")),
        "not"),
      newStmtList(
        newAssignment(
          newDotExpr(
            ident"this",
            ident(inst.id & "Inst")),
          newCall(
            inst.`type`.parsed.strVal,
            inst.value)), # XXX
        newAssignment(
          newDotExpr(
            ident"this",
            ident(inst.id & "Cached")),
          ident"true")))),
    nnkReturnStmt.newTree(
      newDotExpr(
        ident"this",
        ident(inst.id & "Inst"))))

proc parentType(node: Type): string =
  if node.supertype == nil: rootTypeName else: node.supertype.id

proc typeDecl(node: Type): NimNode =
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

  result = nnkTypeDef.newTree(
    ident(node.id),
    newEmptyNode(),
    nnkRefTy.newTree(
      nnkObjectTy.newTree(
        newEmptyNode(),
        nnkOfInherit.newTree(
          ident(rootTypeName)),
        fields)))

proc typeSection(node: Type): NimNode =
  result = newTree(nnkTypeSection)
  result.add(typeDecl(node))
  for t in node.types:
    result.add(typeDecl(t))

proc readProc(node: Type): NimNode =
  result = newProc(name = ident"read")

  result.params = nnkFormalParams.newTree(
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
      ident(rootTypeName),
      newNilLit()),
    newIdentDefs(
      ident"parent",
      ident(parentType(node)),
      newNilLit()))

  var
    parseAttrs = newStmtList()
    constructor = nnkObjConstr.newTree(
      ident(node.id),
      newColonExpr(
        ident"io",
        ident"io"),
      newColonExpr(
        ident"parent",
        ident"parent"))

  for a in node.seq:
    parseAttrs.add(parseAttr(a, node.meta.endian))

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
          ident"root"))),
    parseAttrs)

proc procs(node: Type): NimNode =
  result = newStmtList()
  for c in node.types:
    result.add(procs(c))
  for i in node.instances:
    result.add(instanceProc(i, node))
  result.add(readProc(node))

proc fromFileProc(node: Type): NimNode =
  newStmtList(
    nnkProcDef.newTree(
      ident"fromFile",
      newEmptyNode(),
      newEmptyNode(),
      nnkFormalParams.newTree(
        ident(node.id),
        newIdentDefs(
          ident"_",
          nnkBracketExpr.newTree(
            ident"typedesc",
            ident(node.id))),
        newIdentDefs(
          ident"filename",
          ident"string")),
      newEmptyNode(),
      newEmptyNode(),
      newStmtList(
        newCall(
          ident"read",
          ident(node.id),
          newCall(
            ident"newKaitaiFileStream",
            ident"filename")))))

proc fromFileProcs(node: Type): NimNode =
  result = newStmtList()
  if node.types != @[]:
    for c in node.types:
      result.add(fromFileProcs(c))
  result.add(fromFileProc(node))

proc generateParser*(spec: JsonNode): NimNode =
  let spec = spec.toKsType
  result = newStmtList(
    typeSection(spec),
    procs(spec),
    fromFileProcs(spec))
  #echo repr result

# static library
macro injectParser*(spec: static[JsonNode]) =
  generateParser(spec)

# dynamic library
proc createDynlib*(spec: JsonNode, path: string) = discard

# source code
proc outputModule*(spec: JsonNode): string = discard

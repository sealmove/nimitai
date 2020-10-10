import nimitai/ksast
import json, macros, regex

const
  rootTypeName = "KaitaiStruct"
  streamTypeName = "KaitaiStream"

proc nativeType(ksyType: string): string =
  case ksyType
  of "u1": result = "uint8"
  of "s1": result = "int8"
  of "u2": result = "uint16"
  of "s2": result = "int16"
  of "u4": result = "uint32"
  of "s4": result = "int32"
  of "u8": result = "uint64"
  of "s8": result = "int64"
  of "f4": result = "float32"
  of "f8": result = "float64"
  of "str", "strz": result = "string"
  else: # TODO: implement look-up here
    result = ksyType

proc attribute(json: JsonNode): NimNode =
  newIdentDefs(
    ident(json["id"].getStr),
    ident(nativeType(json["type"].getStr)))

proc type(node: KsNode): NimNode =
  var fields = newTree(nnkRecList)

  fields.add(
    newIdentDefs(
      ident"parent",
      ident(rootTypeName)))

  for a in node.seq:
    fields.add(attribute(a))

  result = nnkTypeDef.newTree(
    ident(node.name),
    newEmptyNode(),
    nnkRefTy.newTree(
      nnkObjectTy.newTree(
        newEmptyNode(),
        nnkOfInherit.newTree(
          ident(rootTypeName)),
        fields)))

proc call(json: JsonNode): NimNode =
  let t = json["type"].getStr
  if t.match(re"[us][1248]|f[48]"):
    var procName = "read" & t
    if not t.match(re"[us][1]"):
      procName &= "le" # XXX
    result = newCall(
      procName,
      newDotExpr(
        ident"result",
        ident"io"))
  else:
    result = nil # XXX

proc typeSection(node: KsNode): NimNode =
  result = newTree(nnkTypeSection)
  result.add(type(node))
  for t in node.children:
    result.add(type(t))

proc readProc(node: KsNode): NimNode =
  result = newProc(name = ident"read")

  result.params = nnkFormalParams.newTree(
    ident(node.name),
    newIdentDefs(
      ident"_",
      nnkBracketExpr.newTree(
        ident"typedesc",
        ident(node.name))),
    newIdentDefs(
      ident"io",
      ident(streamTypeName)),
    newIdentDefs(
      ident"root",
      ident(rootTypeName),
      newNilLit()),
    newIdentDefs(
      ident"parent",
      ident(rootTypeName),
      newNilLit()))

  var fieldAsgns = newStmtList()
  for a in node.seq:
    fieldAsgns.add(
      newAssignment(
        newDotExpr(
          ident"result",
          ident(a["id"].getStr)),
          call(a)))

  result.body = newStmtList(
    newAssignment(
      ident"result",
      nnkObjConstr.newTree(
        ident(node.name),
        newColonExpr(
          ident"io",
          ident"io"),
        newColonExpr(
          ident"parent",
          ident"parent"))),
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
    fieldAsgns)

proc fromFileProc(node: KsNode): NimNode =
  newStmtList(
    nnkProcDef.newTree(
      ident"fromFile",
      newEmptyNode(),
      newEmptyNode(),
      nnkFormalParams.newTree(
        ident(node.name),
        newIdentDefs(
          ident"_",
          nnkBracketExpr.newTree(
            ident"typedesc",
            ident(node.name))),
        newIdentDefs(
          ident"filename",
          ident"string")),
      newEmptyNode(),
      newEmptyNode(),
      newStmtList(
        newCall(
          ident"read",
          ident(node.name),
          newCall(
            ident"newKaitaiFileStream",
            ident"filename")))))

proc generateParser*(spec: JsonNode): NimNode =
  let spec = spec.toKsNode
  result = newStmtList(
    typeSection(spec),
    readProc(spec),
    fromFileProc(spec))

# static library
macro injectParser*(spec: static[JsonNode]) =
  generateParser(spec)

# dynamic library
proc createDynlib*(spec: JsonNode, path: string) = discard

# source code
proc outputModule*(spec: JsonNode): string = discard

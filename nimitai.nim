import macros, json, strutils
import nimitai/exprlang

const
  rootTypeName = "KaitaiStruct"
  streamTypeName = "KaitaiStream"

iterator types(json: JsonNode): tuple[name: string, contents: JsonNode] =
  var
    hierarchy = @[json["meta"]["id"].getStr]
    stack = @[(hierarchy, json)]
  while stack.len != 0:
    let (h, curr) = pop(stack)
    if curr.contains("types"):
      for t in curr["types"].pairs:
        hierarchy.add(t.key)
        stack.add((hierarchy, t.val))
        discard pop(hierarchy)
    yield (h.join("_"), curr)

proc nativeType(ksyType: string): string =
  case ksyType
  of "u1": result = "uint8"
  of "s1": result = "int8"
  of "u2", "u2le", "u2be": result = "uint16"
  of "s2", "s2le", "s2be": result = "int16"
  of "u4", "u4le", "u4be": result = "uint32"
  of "s4", "s4le", "s4be": result = "int32"
  of "u8", "u8le", "u8be": result = "uint64"
  of "s8", "s8le", "s8be": result = "int64"
  of "f4", "f4be", "f4le": result = "float32"
  of "f8", "f8be", "f8le": result = "float64"
  of "str", "strz": result = "string"
  else: # TODO: implement look-up here
    result = ksyType

proc inferInstanceType(json: JsonNode): string =
  if json.contains("type"): nativeType(json["type"].getStr)
  else: "auto"

proc attribute(json: JsonNode): NimNode =
  newIdentDefs(
    ident(json["id"].getStr),
    ident(nativeType(json["type"].getStr)))

proc instance(name: string, json: JsonNode): NimNode =
  newIdentDefs(
    ident(name),
    ident(inferInstanceType(json)))

proc type(name: string, json: JsonNode): NimNode =
  var fields = newTree(nnkRecList)

  fields.add(
    newIdentDefs(
      ident"parent",
      ident(rootTypeName)))

  for a in json["seq"]:
    fields.add(attribute(a))

  #for k, v in json["instances"].pairs:
  #  fields.add(instance(k, v))

  result = nnkTypeDef.newTree(
    ident(name),
    newEmptyNode(),
    nnkRefTy.newTree(
      nnkObjectTy.newTree(
        newEmptyNode(),
        nnkOfInherit.newTree(
          ident(rootTypeName)),
        fields)))

proc typeSection(json: JsonNode): NimNode =
  result = newTree(nnkTypeSection)
  for n, c in types(json):
    result.add(type(n, c))

proc readFD(typeName: string): NimNode =
  nnkProcDef.newTree(
    ident"read",
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      ident(typeName),
      newIdentDefs(
        ident"_",
        nnkBracketExpr.newTree(
          ident"typedesc",
          ident(typeName))),
      newIdentDefs(
        ident"io",
        ident(streamTypeName)),
      newIdentDefs(
        ident"root",
        ident(rootTypeName)),
      newIdentDefs(
        ident"parent",
        ident(rootTypeName))),
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode())

proc readProcsFD(json: JsonNode): NimNode =
  result = newStmtList()
  for n, c in types(json):
    result.add(readFD(n))

proc instanceProcFD(typeName, key: string; val: JsonNode): NimNode =
  nnkProcDef.newTree(
    ident(key),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      #expr(val["value"].getStr),
      ident(inferInstanceType(val)),
      newIdentDefs(
        ident"this",
        ident(typeName))),
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode())

proc instanceProcsFD(json: JsonNode): NimNode =
  result = newStmtList()
  for n, c in types(json):
    if c.contains("instances"):
      for k, v in c["instances"].pairs:
        result.add(instanceProcFD(n, k, v))

proc instanceProc(typeName, key: string; val: JsonNode): NimNode =
  result = newProc(name = ident(key))

  result.params = nnkFormalParams.newTree(
    ident(inferInstanceType(val)),
    newIdentDefs(
      ident"this",
      ident(typeName)))

  if val.contains("value"):
    result.body = expr(val["value"].getStr)

proc readProc(typeName: string, json: JsonNode): NimNode =
  result = newProc(name = ident"read")

  result.params = nnkFormalParams.newTree(
    ident(typeName),
    newIdentDefs(
      ident"_",
      nnkBracketExpr.newTree(
        ident"typedesc",
        ident(typeName))),
    newIdentDefs(
      ident"io",
      ident(streamTypeName)),
    newIdentDefs(
      ident"root",
      ident(rootTypeName)),
    newIdentDefs(
      ident"parent",
      ident(rootTypeName)))

  var calls = newStmtList()
  for a in json["seq"]:
    let
      id = a["id"].getStr
      typ = a["type"].getStr

    var procedure: string
    case typ
    of "u1": procedure = "readU1"
    of "u2": procedure = "readU2Le"
    else: procedure = "XXX"

    let call = newCall(
      newDotExpr(
        newDotExpr(
          ident"result",
          ident"io"),
        ident(procedure)))

    calls.add(
      newAssignment(
        newDotExpr(
          ident"result",
          ident(id)),
        call))

  result.body = newStmtList(
    newAssignment(
      ident"result",
      newCall(
        ident"new",
        ident(typeName))),
    newLetStmt(
      ident"root",
      nnkIfExpr.newTree(
        nnkElifExpr.newTree(
          nnkInfix.newTree(
            ident"==",
            ident"root",
            newNilLit()),
          nnkCast.newTree(
            ident(typeName),
            ident"result")),
        nnkElseExpr.newTree(
          nnkCast.newTree(
            ident(typeName),
            ident"root")))),
    newAssignment(
      newDotExpr(
        ident"result",
        ident"io"),
      ident"io"),
    newAssignment(
      newDotExpr(
        ident"result",
        ident"root"),
      ident"root"),
    newAssignment(
      newDotExpr(
        ident"result",
        ident"parent"),
      ident"parent"),
    calls)

proc fromFileProc(typeName: string): NimNode =
  newStmtList(
    nnkProcDef.newTree(
      ident"fromFile",
      newEmptyNode(),
      newEmptyNode(),
      nnkFormalParams.newTree(
        ident(typeName),
        newIdentDefs(
          ident"_",
          nnkBracketExpr.newTree(
            ident"typedesc",
            ident(typeName))),
        newIdentDefs(
          ident"filename",
          ident"string")),
      newEmptyNode(),
      newEmptyNode(),
      newStmtList(
        newCall(
          newDotExpr(
            ident(typeName),
            ident"read"),
          newCall(
            ident"newKaitaiFileStream",
            ident"filename"),
          newNilLit(),
          newNilLit()))))

proc procs(json: JsonNode): NimNode =
  result = newStmtList()
  for n, c in types(json):
    result.add(
      readProc(n, c),
      fromFileProc(n))
    if c.contains("instances"):
      for k, v in c["instances"].pairs:
        result.add(
          instanceProc(n, k, v))

proc generateParser(ksj: string): NimNode =
  let json = parseJson(readFile(ksj))

  result = newStmtList(
    typeSection(json),
    readProcsFD(json),
#    instanceProcsFD(json),
    procs(json))

macro injectParser*(ksj: static[string]) =
  result = generateParser(ksj)
#  echo repr result

proc writeModule(ksj, module: string) =
  writeFile(module, generateParser(ksj).repr)

proc writeDll(ksj, dll: string) = discard


# debugging
static:
  const t = "testing/specs/expr_0.ksj"
  echo parseJson(readFile(t)).pretty
  echo repr generateParser(t)

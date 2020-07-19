import macros, json, strutils

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

proc field(json: JsonNode): NimNode =
  newIdentDefs(
    ident(json["id"].getStr),
    ident(nativeType(json["type"].getStr)))

proc type(name: string, json: JsonNode): NimNode =
  var fields = newTree(nnkRecList)

  for f in json["seq"]:
    fields.add(field(f))

  fields.add(
    newIdentDefs(
      ident"parent",
      ident(rootTypeName)))

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

proc readForwardDeclaration(typeName: string): NimNode =
  result = nnkProcDef.newTree(
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

proc readForwardDeclarations(json: JsonNode): NimNode =
  result = newStmtList()
  for n, c in types(json):
    result.add(
      readForwardDeclaration(n))

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

  result.body =
    newStmtList(
      nnkTemplateDef.newTree(
        ident"this",
        newEmptyNode(),
        newEmptyNode(),
        nnkFormalParams.newTree(
          ident"untyped"),
        newEmptyNode(),
        newEmptyNode(),
        ident"result"),
      newAssignment(
        ident"this",
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
              ident"this")),
          nnkElseExpr.newTree(
            nnkCast.newTree(
              ident(typeName),
              ident"root")))),
      newAssignment(
        newDotExpr(
          ident"this",
          ident"io"),
        ident"io"),
      newAssignment(
        newDotExpr(
          ident"this",
          ident"root"),
        ident"root"),
      newAssignment(
        newDotExpr(
          ident"this",
          ident"parent"),
        ident"parent"))

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

proc generateParser(ksj: string): NimNode =
  let json = parseJson(readFile(ksj))

  result = newStmtList(
    typeSection(json),
    #readForwardDeclarations(json),
    procs(json))

  # debugging
  # echo repr result

macro injectParser*(ksj: static[string]) =
  result = generateParser(ksj)

proc writeModule(ksj, module: string) =
  writeFile(module, generateParser(ksj).repr)

proc writeDll(ksj, dll: string) = discard

# debugging
#static:
#  discard generateParser("testing/specs/hello_world.ksj")

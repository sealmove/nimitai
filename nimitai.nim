import macros, json, strutils

const
  rootTypeName = "KaitaiStruct"

iterator types(json: JsonNode): tuple[name: string, contents: JsonNode] =
  var
    hierarchy = @[json["meta"]["id"].getStr]
    stack = @[(json, hierarchy)]
  while stack.len != 0:
    let (curr, h) = pop(stack)
    for t in curr["types"].pairs:
      if t.val.contains("types"):
        hierarchy.add(t.key)
        stack.add((t.val, hierarchy))
        discard pop(hierarchy)
      yield (h.join("_") & "_" & t.key, t.val)

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

proc typeSection(json: JsonNode): NimNode =
  result = newTree(nnkTypeSection)

  var fields = newTree(nnkRecList)

  for f in json["seq"]:
    fields.add(
      newIdentDefs(
        ident(f["id"].getStr),
        ident(nativeType(f["type"].getStr))))

  fields.add(
    newIdentDefs(
      ident"parent",
      ident(rootTypeName)))

  result.add(nnkTypeDef.newTree(
    ident(json["meta"]["id"].getStr),
    newEmptyNode(),
    nnkRefTy.newTree(
      nnkObjectTy.newTree(
        newEmptyNode(),
        nnkOfInherit.newTree(
          ident(rootTypeName)),
        fields))))

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
        ident"KaitaiStream"),
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
  result.add(
    readForwardDeclaration(json["meta"]["id"].getStr))

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
      ident(rootTypeName)),
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
          ident"HelloWorld")),
      newLetStmt(
        ident"root",
        nnkIfExpr.newTree(
          nnkElifExpr.newTree(
            nnkInfix.newTree(
              ident"==",
              ident"root",
              newNilLit()),
            nnkCast.newTree(
              ident"HelloWorld",
              ident"this")),
          nnkElseExpr.newTree(
            nnkCast.newTree(
              ident"HelloWorld",
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

proc readProcs(json: JsonNode): NimNode =
  result = newStmtList()
  for t in json["types"].pairs:
    result.add(
      readProc(t.key, t.val))

proc generateParser(ksj: string): NimNode =
  let json = parseJson(readFile(ksj))
  for k, c in json.types():
    echo k
    echo "--------"
    echo pretty(c)

  result = newStmtList(
    typeSection(json),
    readForwardDeclarations(json),
    readProcs(json))

  # debugging
  #echo repr result

macro injectParser(ksj: static[string]) =
  result = generateParser(ksj)

proc writeModule(ksj, module: string) =
  writeFile(module, generateParser(ksj).repr)

proc writeDll(ksj, dll: string) = discard

# debugging
static:
  discard generateParser("ksj/nested_types.ksj")

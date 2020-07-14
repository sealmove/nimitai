import macros, json

const
  rootTypeName = "KaitaiStruct"

proc typeSection(json: JsonNode): NimNode =
  result = newTree(nnkTypeSection)

  var fields = newTree(nnkRecList)

  for f in json["seq"]:
    fields.add(
      nnkIdentDefs.newTree(
        ident(f["id"].getStr),
        ident(f["type"].getStr),
        newEmptyNode()))

  fields.add(
    nnkIdentDefs.newTree(
      newIdentNode("parent"),
      newIdentNode(rootTypeName),
      newEmptyNode()))

  result.add(nnkTypeDef.newTree(
    ident(json["meta"]["id"].getStr),
    newEmptyNode(),
    nnkRefTy.newTree(
      nnkObjectTy.newTree(
        newEmptyNode(),
        nnkOfInherit.newTree(
          ident(rootTypeName)),
        fields))))

  # debugging
  echo json.pretty

proc generateParser(ksj: string): NimNode =
  let json = parseJson(readFile(ksj))

  result = newStmtList(
    typeSection(json))

  # debugging
  echo repr result

macro injectParser(ksj: static[string]) =
  result = generateParser(ksj)

proc writeModule(ksj, module: string) =
  writeFile(module, generateParser(ksj).repr)

proc writeDll(ksj, dll: string) = discard

# debugging
static:
  discard generateParser("ksj/hello_world.ksj")

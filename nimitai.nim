import macros, json

const
  rootTypeName = "KaitaiStruct"

proc generateParser(ksj: string): NimNode =
  let json = parseJson(readFile(ksj))
  result = nnkTypeSection.newTree(
    nnkTypeDef.newTree(
      ident(json["meta"]["id"].getStr),
      newEmptyNode(),
      nnkRefTy.newTree(
        nnkObjectTy.newTree(
          newEmptyNode(),
          nnkOfInherit.newTree(
            ident(rootTypeName)),
          nnkRecList.newTree(
            nnkIdentDefs.newTree(
              newIdentNode("one"),
              newIdentNode("uint8"),
              newEmptyNode()
            ),
            nnkIdentDefs.newTree(
              newIdentNode("parent"),
              newIdentNode("KaitaiStruct"),
              newEmptyNode()))))))

  echo repr result

macro injectParser(ksj: static[string]) =
  result = generateParser(ksj)

proc writeModule(ksj, module: string) =
  writeFile(module, generateParser(ksj).repr)

proc writeDll(ksj, dll: string) = discard

static:
  discard generateParser("ksj/hello_world.ksj")

## Compiles and runs a unittest suite using the kst files

import macros, json, strformat

proc imports(): NimNode =
  nnkImportStmt.newTree(
    ident"nimitai",
    ident"kaitai_struct_nim_runtime")

proc test(json: JsonNode): NimNode =
  let
    id = json["id"].getStr
    data = json["data"].getStr

  var stmts = newStmtList(
    newLetStmt(
      newIdentNode("r"),
      newCall(
        newDotExpr(
          ident(id),
          newIdentNode("fromFile")),
        newLit(&"subjects/{data}"))))

  for a in json["asserts"]:
    stmts.add(
      nnkCall.newTree(
        ident"check",
        infix(
          newDotExpr(
            ident"r",
            ident(a["actual"].getStr)),
          "==",
          newLit(a["expected"].getStr))))

  result = nnkCommand.newTree(
    newIdentNode("test"),
    newLit(id),
    stmts)

proc suite(): NimNode =
  let json = parseJson(readFile("tests/hello_world.kst"))
  newStmtList(
    imports(),
    test(json))

static:
  echo repr suite()

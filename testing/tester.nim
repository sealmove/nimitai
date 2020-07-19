## Compiles and runs a unittest suite using the kst files

import macros, json, strformat, oswalkdir
import ../nimitai/exprlang

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
      newCall(
        ident"check",
        infix(
          newDotExpr(
            ident"r",
            ident(a["actual"].getStr)),
          "==",
          expr(a["expected"].getStr))))

  result = nnkCommand.newTree(
    newIdentNode("test"),
    newLit(id),
    stmts)

proc suite(): NimNode =
  var tests = newStmtList()
  for a, f in walkDir("tests/working_tests"):
    let json = parseJson(readFile(f))
    tests.add(test(json))

  newStmtList(
    nnkImportStmt.newTree(
      ident"nimitai",
      ident"kaitai_struct_nim_runtime"),
    nnkCommand.newTree(
      ident"suite",
      newLit"Nimitai test suite",
      tests))

static:
  echo repr suite()

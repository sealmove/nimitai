## Compiles and runs a unittest suite using the kst files

import macros, json, strformat, oswalkdir
import ../nimitai/exprlang

proc test(json: JsonNode): NimNode =
  let
    id = json["id"].getStr
    data = json["data"].getStr

  var stmts = newStmtList(
    newCall(
      ident"injectParser",
      newLit(&"testing/specs/{id}.ksj")),
    newLetStmt(
      newIdentNode("r"),
      newCall(
        newDotExpr(
          ident(id),
          newIdentNode("fromFile")),
        newLit(&"testing/subjects/{data}"))))

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

  nnkCommand.newTree(
    newIdentNode("test"),
    newLit(id),
    stmts)

macro suite() =
  var tests = newStmtList()
  for a, f in walkDir("testing/tests/working_tests"):
    let json = parseJson(readFile(f))
    tests.add(test(json))

  newStmtList(
    nnkImportStmt.newTree(
      ident"nimitai",
      ident"kaitai_struct_nim_runtime",
      ident"unittest"),
    nnkCommand.newTree(
      ident"suite",
      newLit"Nimitai Test Suite",
      tests))

suite()

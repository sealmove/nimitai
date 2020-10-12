## Compiles and runs a unittest suite using the kst files

import macros, json, strformat, os, strutils
import ../nimitai_compiler/nimitai/exprlang
import json6

proc test(json: JsonNode): NimNode =
  let
    id = json["id"].getStr
    data = json["data"].getStr

  var stmts = newStmtList(
    newCall(
      ident"injectParser",
      newCall(
        ident"parseJson",
        newCall(
          ident"readFile",
          newLit(&"specs/{id}.ksj")))),
    newLetStmt(
      newIdentNode("r"),
      newCall(
        newDotExpr(
          ident(id.capitalizeAscii),
          newIdentNode("fromFile")),
        newLit(&"subjects/{data}"))))

  if json.hasKey("asserts"):
    for a in json["asserts"]:
      let expected = a["expected"]
      var nodeExpected: NimNode
      case expected.kind
      of JString:
        nodeExpected = expr(expected.getStr)
      of JInt:
        nodeExpected = newLit(expected.getInt)
      of JBool:
        nodeExpected = newLit(expected.getBool)
      of JNull:
        nodeExpected = newNilLit()
      else: discard # Should not occur
      stmts.add(
        newCall(
          ident"check",
          infix(
            expr("r." & a["actual"].getStr),
            "==",
            nodeExpected)))

  nnkCommand.newTree(
    newIdentNode("test"),
    newLit(id),
    stmts)

macro suite() =
  var tests = newStmtList()
  for a, f in walkDir("tests/working_tests"):
    let json = parseJson6(readFile(f))
    tests.add(test(json))

  result = newStmtList(
    nnkImportStmt.newTree(
      ident"json",
      ident"options",
      ident"../nimitai_compiler/nimitai",
      ident"../kaitai_struct_nim_runtime/kaitai_struct_nim_runtime",
      ident"unittest"),
    nnkCommand.newTree(
      ident"suite",
      newLit"Nimitai Test Suite",
      tests))

suite()

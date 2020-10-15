import os, strformat, strutils, macros, json
import ../src/nimitai/exprlang

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

  newStmtList(
    nnkImportStmt.newTree(
      ident"json",
      ident"options",
      ident"../../../src/nimitai",
      ident"../../../src/nimitai/runtime",
      ident"unittest"),
    nnkCommand.newTree(
      ident"suite",
      newLit"Nimitai Test Suite",
      newStmtList(
        nnkCommand.newTree(
          newIdentNode("test"),
          newLit(id),
          stmts))))

const
  R = "\e[31;1m"
  G = "\e[32;1m"
  Y = "\e[33;1m"
  B = "\e[34;1m"
  D = "\e[0m"

# OK: Test was generated successfully
# JE: Json error (Failed to parse .kst file)
# LE: Lexing error (Failed to lex some expression in the test)
# PE: Parsing error (Failed to parse some expression in the test)

static:
  echo &"{B}[Generating]{D} Nimitai"
  for k, f in walkDir("tests"):
    if k == pcDir: continue

    let casename = splitFile(f).name
    var
      json: JsonNode
      ast: Nimnode

    echo &"  {G}[OK]{D} " & casename

    try:
      json = parseJson(readFile(f))
    except JsonParsingError:
      echo &"  {B}[JE]{D} " & casename
      continue

    try:
      ast = test(json)
    except LexingError:
      echo &"  {R}[LE]{D} " & casename
      continue

    except ParsingError:
      echo &"  {Y}[PE]{D} " & casename
      continue

    let module = &"tests/compiled/{casename}.nim"
    writeFile(module, repr(ast))

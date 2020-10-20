import os, strformat, strutils, macros, json, algorithm
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
        nodeExpected = expr(expected.getStr, "")
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
            expr("r." & a["actual"].getStr, ""),
            "==",
            nodeExpected)))

  newStmtList(
    nnkImportStmt.newTree(
      ident"json",
      ident"../../testutils",
      infix(
        ident"../../../src",
        "/",
        nnkBracket.newTree(
          ident"nimitai",
          ident"nimitai/runtime")),
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
  M = "\e[35;1m"
  D = "\e[0m"

# OK: Test was generated successfully
# JE: Json error (Failed to parse .kst file)
# LE: Lexing error (Failed to lex some expression in the test)
# PE: Parsing error (Failed to parse some expression in the test)

static:
  var
    ok, je, pe: int
    testFiles: seq[string]

  echo &"{B}[Generating]{D} Nimitai"

  for k, f in walkDir("tests"):
    if k == pcDir: continue
    testFiles.add(f)

  sort(testFiles)

  for f in testFiles:
    let casename = splitFile(f).name
    var
      json: JsonNode
      ast: Nimnode

    try:
      json = parseJson(readFile(f))
    except JsonParsingError:
      echo &"  {R}[JE]{D} " & casename
      inc(je)
      continue

    try:
      ast = test(json)
    except ParsingError:
      echo &"  {M}[PE]{D} " & casename
      inc(pe)
      continue

    echo &"  {G}[OK]{D} " & casename
    inc(ok)
    writeFile(&"tests/compiled/{casename}.nim",
              repr(ast).splitLines[1..^1].join("\n"))

  echo "----------------------------------------\n" &
       &"  {G}[OK]{D} {ok.intToStr(3)}\n" &
       &"  {R}[JE]{D} {je.intToStr(3)}\n" &
       &"  {M}[PE]{D} {pe.intToStr(3)}\n" &
       "----------------------------------------\n"

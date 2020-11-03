import os, strformat, strutils, macros, json, algorithm
import ../src/nimitai/[ksast, exprlang, identutils]

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
    nnkVarSection.newTree(
      newIdentDefs(
        ident"this",
        ident(id.capitalizeAscii))))

  var userObjLetStmt =
    newAssignment(
      newIdentNode("this"),
      newCall(
        newDotExpr(
          ident(id.capitalizeAscii),
          newIdentNode("fromFile")),
        newLit(&"subjects/{data}")))

  if json.hasKey("exception"):
    let exception = ident(json["exception"].getStr)
    userObjLetStmt = nnkTryStmt.newTree(
      userObjLetStmt,
      nnkExceptBranch.newTree(
        exception,
        nnkDiscardStmt.newTree(newEmptyNode())))

  stmts.add(userObjLetStmt)

  if json.hasKey("asserts"):
    for a in json["asserts"]:
      let actual = a["actual"].getStr.toKs(nil).toNim
      var expected: NimNode
      case a["expected"].kind
      of JString:
        expected = a["expected"].getStr.toKs(nil).toNim
      of JInt:
        expected = newLit(a["expected"].getInt)
      of JFloat:
        expected = newLit(a["expected"].getFloat)
      of JBool:
        expected = newLit(a["expected"].getBool)
      of JNull:
        expected = newNilLit()
      else: discard # Should not occur
      stmts.add(
        newCall(
          ident"check",
          if expected.kind == nnkIdent and eqIdent(expected, "nil"):
            actual[^1] = isParsedId(actual[^1].strval)
            infix(actual, "==", ident"false")
          else:
            infix(actual, "~=", expected)))

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
    except JsonParsingError, ValueError:
      echo &"  {R}[JE]{D} " & casename & &" {R}{getCurrentException().msg}{D}"
      inc(je)
      continue

    try:
      ast = test(json)
    except ParsingError as err:
      echo &"  {M}[PE]{D} " & casename & &" {M}{err.msg}{D}"
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

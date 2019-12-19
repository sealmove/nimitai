import
  npeg, strutils, strformat, sequtils, macros, oswalkdir,
  ../../nimitai, ../../nimitai/private/ast
from os import extractFilename

type Kst = object
  id: string
  data: string
  asserts: seq[tuple[actual, expected: string]]

const
  R = "\e[31;1m"
  G = "\e[32;1m"
  Y = "\e[33;1m"
  B = "\e[34;1m"
  D = "\e[0m"

proc parseKst(path: string): Kst =
  let p = peg(kst, test: Kst):
    K(item) <- item * *Blank * ':' * *Blank
    kst <- *'\n' * Id * +'\n' * Data * +'\n' * ?(Imports * +'\n') *
           ?(Asserts|Expection) * *'\n' * !1
    Id <- K("id") * >Line:
      test.id = $1
    Data <- K("data") * >Line:
      test.data = $1
    Imports <- K("imports") * Line
    Expection <- K("exception") * Line
    Asserts <- K("asserts") * +(+'\n' * Pair)
    Pair <- Actual * >Line * +'\n' * Expected * >Line:
      test.asserts.add ($1, $2)
    Line <- +(1 - '\n')
    Actual <- *' ' * '-' * *' ' * K("actual")
    Expected <- *' ' * K("expected")

  let file = readFile(path).splitLines
                           .filterIt(not it.strip.startsWith('#'))
                           .join("\n")
  var test: Kst
  doAssert p.match(file, test).ok
  test

proc test(kst: Kst): NimNode =
  var asserts = newStmtList()

  for a in kst.asserts:
    asserts.add(
      nnkCommand.newTree(
        ident"check",
        newCall(
          ident"coEq",
          newDotExpr(
            ident"r",
            ident(a.actual)),
          parseKsExpr(a.expected).toNim)))

  nnkCommand.newTree(
    ident"test",
    newLit(kst.id),
    newStmtList(
      newCall(
        ident"injectParser",
        newLit("../material/ksy/" & kst.id & ".ksy")),
      newLetStmt(
        ident"r",
        newCall(
          ident"fromFile",
          ident(kst.id.capitalizeAscii),
          newLit("../material/bin/" & kst.data))),
      asserts))

proc suite(tests: varargs[NimNode], errorCode = -1): string =
  var res = newStmtList(
    nnkImportStmt.newTree(
      ident"../../nimitai",
      ident"../../../kaitai_struct_nim_runtime/kaitai_struct_nim_runtime",
      ident"unittest",
      ident"options"),
    nnkPragma.newTree(
      newColonExpr(
        ident"experimental",
        newLit("dotOperators"))),
    nnkCommand.newTree(
        ident"suite",
        newLit("Nimitai Test Suite"),
        newStmtList().add(tests)))
  if errorCode != -1:
    res.add(
      newCall(
        ident"quit",
        newLit(errorCode)))

  res.toStrLit.strVal

# [CC] = FAILED TO COMPILE
# [CE] = CODGEN ERROR
# [RC] = RUNTIME CRASH
proc siftedSuite(): string =
  var
    tests: seq[NimNode]
    CC, CE, RC, OK: int
  for k, p in walkDir("../material/kst/"):
    var name = extractFilename(p)
    name.removeSuffix(".kst")
    if k == pcFile:
      try:
        writeFile("temp.nim", suite(p.parseKst.test, 2))
        let (ccOut, ccCode) = gorgeEx("nim c --hints:off temp")
        if ccCode != 0:
          inc CC
          echo &"{R}[CC]{D} {name}"
          writeFile(&"../log/cc/{name}", ccOut)
          continue
        let (rcOut, rcCode) = gorgeEx("./temp")
        if rcCode == 2:
          tests.add(p.parseKst.test)
          inc OK
          echo &"{G}[OK]{D} {name}"
        else:
          inc RC
          echo &"{B}[RC]{D} {name}"
          writeFile(&"../log/rc/{name}", rcOut)
      except:
        inc CE
        echo &"{Y}[CE]{D} {name}"
        writeFile(&"../log/ce/{name}", getCurrentExceptionMsg())

  echo ""
  echo &"{G}[OK]{D} {OK.intToStr(3)} {B}[RC]{D} {RC.intToStr(3)} " &
       &"{Y}[CE]{D} {CE.intToStr(3)} {R}[CC]{D} {CC.intToStr(3)}"

  suite(tests)

const code = siftedSuite()
writeFile("testsuite.nim", code)

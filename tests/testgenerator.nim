import npeg, strutils, sequtils, macros, oswalkdir

type Kst = object
  id: string
  data: string
  asserts: seq[tuple[actual, expected: string]]

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

proc parseKsExpr(expr: string): NimNode =
  #XXX
  parseExpr(expr)

proc test(kst: Kst): NimNode =
  var asserts = newStmtList()

  for a in kst.asserts:
    asserts.add(
      nnkCommand.newTree(
        ident"check",
        infix(
          newDotExpr(
            ident"r",
            ident(a.actual)),
          "==",
          parseKsExpr(a.expected))))

  nnkCommand.newTree(
    ident"test",
    newLit(kst.id),
    newStmtList(
      newCall(
        ident"generateParser",
        newLit("material/ksy/" & kst.id & ".ksy")),
      newLetStmt(
        ident"r",
        newCall(
          ident"fromFile",
          ident(kst.id.capitalizeAscii),
          newLit("material/bin/" & kst.data))),
      asserts))

proc suite(): NimNode =
  var tests = newStmtList()
  for k, p in walkDir("material/kst"):
    if k == pcFile:
      tests.add(p.parseKst.test)

  newStmtList(
    nnkImportStmt.newTree(
      ident"nimitai",
      ident"kaitai_struct_nim_runtime",
      ident"unittest"),
    nnkCommand.newTree(
        ident"suite",
        newLit("Nimitai Test Suite"),
        tests))

const code = suite().toStrLit.strVal.strip
writeFile("testsuite.nim", code)

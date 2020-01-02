import
  npeg, strutils, sequtils, macros

type Kst* = object
  id*: string
  data*: string
  asserts*: seq[tuple[actual, expected: string]]

proc parseKst*(path: string): Kst =
  let p = peg(kst, test: Kst):
    K(item) <- item * *Blank * ':' * *Blank
    kst <- *'\n' * Id * +'\n' * Data * +'\n' * ?(Imports * +'\n') *
           ?(Asserts | Expection) * *'\n' * !1
    Id <- K("id") * >Line:
      test.id = $1
    Data <- K("data") * >Line:
      test.data = $1
    Imports <- K("imports") * Line
    Expection <- K("exception") * Line
    Asserts <- K("asserts") * +(+'\n' * Pair)
    Pair <- Actual * >Line * +'\n' * Expected * >Line:
      let a = if ($2).startsWith("'"): ($2)[1..^2] else: $2
      test.asserts.add ($1, a)
    Line <- +(1 - '\n')
    Actual <- *' ' * '-' * *' ' * K("actual")
    Expected <- *' ' * K("expected")

  let file = readFile(path).splitLines
                           .filterIt(not it.strip.startsWith('#'))
                           .join("\n")
  var test: Kst
  doAssert p.match(file, test).ok
  test

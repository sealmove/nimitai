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
    Id <- K("id") * >Item * ?Comment:
      test.id = $1
    Data <- K("data") * >Item * ?Comment:
      test.data = $1
    Imports <- K("imports") * Item * ?Comment
    Expection <- K("exception") * Item * ?Comment
    Asserts <- K("asserts") * +(+'\n' * Pair)
    Pair <- Actual * >Item * ?Comment * +'\n' * Expected * >Item * ?Comment:
      var
        a = ($1).strip
        b = ($2).strip
      b = if b.startsWith("'"): b[1..^2] else: b
      test.asserts.add (a, b)
    Item <- +(Print - {'\n', '#'})
    Comment <- '#' * +(Print - '\n')
    Actual <- *' ' * '-' * *' ' * K("actual")
    Expected <- *' ' * K("expected")

  let file = readFile(path).splitLines
                           .filterIt(not it.strip.startsWith('#'))
                           .join("\n")
  var test: Kst
  doAssert p.match(file, test).ok
  test

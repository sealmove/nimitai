# Kaitai Struct Expression Language

import parseutils, macros, strformat
import strutils except parseBiggestInt
import npeg

type
  KsNodeKind* = enum
    knkBool
    knkInt
    knkFloat
    knkStr
    knkOp
    knkId
    knkEnum
    knkCast
    knkArr
    knkMeth
    knkIdx
    knkDotExpr
    knkUnary
    knkInfix
    knkTernary
  KsNode* = ref object
    case kind*: KsNodeKind
    of knkBool:
      boolval*: bool
    of knkInt:
      intval*: BiggestInt
    of knkFloat:
      floatval*: float
    of knkEnum, knkCast:
      scope*: seq[string]
    of knkStr, knkOp, knkId:
      strval*: string
    else:
      sons*: seq[KsNode]
  ParsingError* = object of CatchableError

proc isFatherKind(kind: KsNodeKind): bool =
  kind in {knkArr, knkMeth, knkIdx, knkDotExpr, knkUnary, knkInfix, knkTernary}

proc add(node: KsNode, children: varargs[KsNode]) =
  for c in children:
    node.sons.add(c)

proc newKsNode*(kind: KsNodeKind, children: varargs[KsNode]): KsNode =
  doAssert isFatherKind(kind)
  result = KsNode(kind: kind)
  for c in children:
    result.add(c)

proc `[]`(node: KsNode, index: int): KsNode =
  doAssert isFatherKind(node.kind)
  node.sons[index]

proc toKs*(str: string): KsNode =
  let p = peg(G, s: seq[seq[KsNode]]):
    # Non-terminal
    G         <- S * expr * !1
    expr      <- S * prefix * *infix
    prefix    <- arr | meth | idx | unary | parExpr |
                 tBool | tFloat | tInt | tStr | tCast | tEnum | tId
    arr       <- '[' * S * newLvl * *(expr * *(',' * expr)) * S * ']' * S:
      let
        elements = pop(s)
        newNode = newKsNode(knkArr)
      for e in elements:
        newNode.add(e)
      s[^1].add newNode
    meth      <- >("to_s"|"to_i"|"length"|"substring"|"size"|"first"|"last") *
                 &(!(Alpha | '_' | '[')) * newLvl * ?('(' * expr * *(',' * expr) * ')') * S:
      let
        elements = pop(s)
        newNode = newKsNode(knkMeth, KsNode(kind: knkId, strval: $1))
      for e in elements:
        newNode.add(e)
      s[^1].add newNode
    idx       <- >id * S * '[' * expr * ']' * S:
      s[^1].add newKsNode(knkIdx, KsNode(kind: knkId, strval: $1), pop(s[^1]))
    unary     <- >{'+','-'} * expr * S:
      s[^1].add newKsNode(knkUnary, KsNode(kind: knkOp, strval: $1), pop(s[^1]))
    parExpr   <- ('(' * expr * ')') * S ^ 0
    infix     <- >("?") * S * expr * ":" * S              * expr ^  1 |
                 >("not")                                 * expr ^  2 |
                 >("or" | "^")                            * expr ^  3 |
                 >("and")                                 * expr ^  4 |
                 >(">=" | ">" | "<=" | "<" | "==" | "!=") * expr ^  5 |
                 >("<<" | ">>" | "&" | "|")               * expr ^  6 |
                 >("+" | "-")                             * expr ^  7 |
                 >("*" | "/")                             * expr ^  8 |
                 >("%")                                   * expr ^  9 |
                 >(".")                                   * expr ^ 10:
      case $1
      of ".":
        let (r, l) = (pop(s[^1]), pop(s[^1]))
        s[^1].add newKsNode(knkDotExpr, l, r)
      of "?":
        let (second, first, condition) = (pop(s[^1]), pop(s[^1]), pop(s[^1]))
        s[^1].add newKsNode(knkTernary, condition, first, second)
      else:
        let (r, l) = (pop(s[^1]), pop(s[^1]))
        s[^1].add newKsNode(knkInfix, l, KsNode(kind: knkOp, strval: $1), r)

    # Terminal
    tBool     <- >("true" | "false") * S:
      s[^1].add KsNode(kind: knkBool, boolval: parseBool($1))
    tFloat    <- >(int * '.' * int * ?('e' * ?{'+', '-'} * int)) * S:
      var f: BiggestFloat
      assert len($1) == parseBiggestFloat($1, f)
      s[^1].add KsNode(kind: knkFloat, floatval: f)
    tInt      <- (tBin | tOct | tHex | tDec) * S
    tBin      <- bin:
      var b: BiggestInt
      assert len($0) == parseBin[BiggestInt]($0, b)
      s[^1].add KsNode(kind: knkInt, intval: b)
    tOct      <- oct:
      var o: BiggestInt
      assert len($0) == parseOct[BiggestInt]($0, o)
      s[^1].add KsNode(kind: knkInt, intval: o)
    tDec      <- dec:
      var d: BiggestInt
      assert len($0) == parseBiggestInt($0, d)
      s[^1].add KsNode(kind: knkInt, intval: d)
    tHex      <- hex:
      var x: BiggestInt
      assert len($0) == parseHex[BiggestInt]($0, x)
      s[^1].add KsNode(kind: knkInt, intval: x)
    tStr      <- (('\"' * >*(Print - '\"') * '\"') |
                  ('\'' * >*(Print - '\'') * '\'')) * S:
      s[^1].add KsNode(kind: knkStr, strval: $1)
    tId       <- >id * S:
      s[^1].add KsNode(kind: knkId, strval: $1)
    tEnum     <- >(id * "::" * id * *("::" * id)) * S:
      s[^1].add KsNode(kind: knkEnum, scope: split($1, "::"))
    tCast     <- "as<" * S * >(id * *("::" * id)) * S * '>' * S:
      s[^1].add KsNode(kind: knkCast, scope: split($1, "::"))

    # Aux
    S        <- *Space
    int      <- hex | oct | bin | dec
    bin      <- "0b" * +{'0', '1', '_'}
    oct      <- "0o" * +{'0' .. '7', '_'}
    dec      <- Digit * *(?'_' * Digit)
    hex      <- "0x" * +(Xdigit | '_')
    id       <- (Lower | '_') * *(Alnum | '_')

    # Aux with side-effects
    newLvl   <- 0:
      s.add newSeq[KsNode]()

  var s = newSeq[seq[KsNode]]()
  s.add newSeq[KsNode]()
  if not p.match(str, s).ok:
    raise newException(ParsingError, str)
  elif s.len == 0:
    raise newException(ParsingError, str & &" (stacks: {s.len})")
  elif s[^1].len != 1:
    raise newException(ParsingError, str & &" (items: {s[0].len})")
  result = s[^1][0]

proc debug(ks: KsNode, n = 0) =
  stdout.write(" ".repeat(n) & ($ks.kind)[3..^1])
  case ks.kind
  of knkEnum, knkCast:
    stdout.write(" " & ks.scope.join(", ") & "\n")
  of knkBool:
    stdout.write(" " & $ks.boolval & "\n")
  of knkInt:
    stdout.write(" " & $ks.intval & "\n")
  of knkFloat:
    stdout.write(" " & $ks.floatval & "\n")
  of knkStr, knkId, knkOp:
    stdout.write(" " & $ks.strval & "\n")
  else:
    stdout.write "\n"
    for s in ks.sons:
      debug(s, n + 2)

#debug("\"abc\" + \"123\"".toKs)

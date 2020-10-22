# Kaitai Struct Expression Language

import parseutils, macros
import strutils except parseBiggestInt
import npeg

type
  State = ref object
    pss: seq[seq[NimNode]]
    context: string
  ParsingError* = object of CatchableError

proc expr*(txt, context: string): NimNode =
  let p = peg(G, s: State):
    # Non-terminal
    G         <- S * expr * !1
    expr      <- S * prefix * *infix
    prefix    <- tBool | tFloat | tInt | tStr | tMeth | tId |
                 idx | arr | parExpr
    idx       <- tId * arrOpen * expr * arrClose:
      let (idx, id) = (pop(s.pss[^1]), pop(s.pss[^1]))
      s.pss[^1].add nnkBracketExpr.newTree(id, idx)
    arr       <- arrOpen * newLvl * expr * *(',' * expr) * arrClose:
      let upperLvl = pop(s.pss)
      var arr = newTree(nnkBracket)
      for n in upperLvl:
        arr.add n
      s.pss[^1].add prefix(arr, "@")
    parExpr   <- (parOpen * expr * parClose) ^ 0
    infix     <- >("not")                                 * expr ^  1 |
                 >("or" | "^")                            * expr ^  2 |
                 >("and")                                 * expr ^  3 |
                 >(">=" | ">" | "<=" | "<" | "==" | "!=") * expr ^  4 |
                 >("<<" | ">>" | "&" | "|")               * expr ^  5 |
                 >("+" | "-")                             * expr ^  6 |
                 >("*" | "/")                             * expr ^  7 |
                 >("%")                                   * expr ^  8 |
                 >(".")                                   * expr ^^ 9 |
                 >("::")                                  * expr ^^ 10:
      let (r, l) = (pop(s.pss[^1]), pop(s.pss[^1]))
      case $1
      of ".":
        s.pss[^1].add newDotExpr(l, r)
      else:
        var op: string
        case $1
        of "%" : op = "mod"
        of "<<": op = "shl"
        of ">>": op = "shr"
        of "&" : op = "and"
        of "|" : op = "or"
        of "^" : op = "xor"
        else   : op = $1
        s.pss[^1].add infix(l, op, r)

    # Terminal
    tBool     <- >("true" | "false") * S:
      s.pss[^1].add newLit(parseBool($1))
    tFloat    <- >(int * '.' * int * ?('e' * ?{'+', '-'} * int)) * S:
      var f: BiggestFloat
      assert len($1) == parseBiggestFloat($1, f)
      s.pss[^1].add newFloatLitNode(f)
    tInt      <- (tBin | tOct | tHex | tDec) * S
    tBin      <- bin:
      var b: BiggestInt
      assert len($0) == parseBin[BiggestInt]($0, b)
      s.pss[^1].add newIntLitNode(b)
    tOct      <- oct:
      var o: BiggestInt
      assert len($0) == parseOct[BiggestInt]($0, o)
      s.pss[^1].add newIntLitNode(o)
    tDec      <- dec:
      var d: BiggestInt
      assert len($0) == parseBiggestInt($0, d)
      s.pss[^1].add newIntLitNode(d)
    tHex      <- hex:
      var x: BiggestInt
      assert len($0) == parseHex[BiggestInt]($0, x)
      s.pss[^1].add newIntLitNode(x)
    tStr      <- '\"' * >*(Print - '\"') * '\"' * S:
      s.pss[^1].add newStrLitNode(($1))
    tMeth     <- >("to_s" | "to_i" | "length" | "reverse" | "substring" |
                 "first" | "last" | "size" | "min" | "max") * S:
      s.pss[^1].add ident($1)
    tId       <- >((Lower | '_') * *(Alnum | '_')) * S:
      if s.context != "":
        s.pss[^1].add newDotExpr(ident(s.context), ident($1))
      else:
        s.pss[^1].add ident($1)

    # Aux
    S        <- *Space
    comma    <- "," * S
    parOpen  <- "(" * S
    parClose <- ")" * S
    arrOpen  <- "[" * S
    arrClose <- "]" * S
    int      <- hex | oct | bin | dec
    bin      <- "0b" * +{'0', '1', '_'}
    oct      <- "0o" * +{'0' .. '7', '_'}
    dec      <- Digit * *(?'_' * Digit)
    hex      <- "0x" * +(Xdigit | '_')

    # Aux with side-effects
    newLvl   <- 0:
      s.pss.add newSeq[NimNode]()

  var pss = newSeq[seq[NimNode]]()
  pss.add newSeq[NimNode]()
  var state = State(pss: pss, context: context)
  if not p.match(txt, state).ok:
    raise newException(ParsingError, "Failed to parse expression: " & txt)
  #if s.pss.len != 1:
  #  raise newException(ParsingError, "Stack Length: " & $s.stack.len)
  state.pss[0][0]

proc debug(s: string) =
  let expr = expr(s, "this")
  echo treeRepr expr
  echo ""
  echo repr expr
  echo ""

#static: debug"a.b + e.f.g * h + 3"

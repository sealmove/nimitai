# Kaitai Struct Expression Language

import parseutils, macros, regex
import strutils except parseBiggestInt
import npeg

type
  State = ref object
    stack: seq[NimNode]
    stackCnt: int
    context: string
  ParsingError* = object of CatchableError

proc expr*(txt, context: string): NimNode =
  var s = State(context: context)
  let p = peg(G, s: State):
    # Non-terminal
    G         <- S * expr * !1
    expr      <- S * prefix * *infix
    prefix    <- tBool | tFloat | tInt | tStr | tMeth | tId | parExpr
    parExpr   <- (tParOpen * expr * tParClose) ^ 0
    infix     <- >("not")                                 * expr ^ 1 |
                 >("or" | "^")                            * expr ^ 2 |
                 >("and")                                 * expr ^ 3 |
                 >(">=" | ">" | "<=" | "<" | "==" | "!=") * expr ^ 4 |
                 >("<<" | ">>" | "&" | "|")               * expr ^ 5 |
                 >("+" | "-")                             * expr ^ 6 |
                 >("*" | "/")                             * expr ^ 7 |
                 >("%")                                   * expr ^ 8 |
                 >(".")                                   * expr ^ 9 |
                 >("::")                                  * expr ^ 10:
      let (r, l) = (s.stack.pop(), s.stack.pop())
      case $1
      of ".":
        s.stack.add newDotExpr(l, r)
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
        s.stack.add infix(l, op, r)

    # Terminal
    tComma    <- "," * S
    tParOpen  <- "(" * S
    tParClose <- ")" * S
    tArrOpen  <- "[" * S
    tArrClose <- "]" * S
    tInt      <- (tBin | tOct | tHex | tDec) * S
    tBin      <- bin:
      var b: BiggestInt
      assert len($0) == parseBin[BiggestInt]($0, b)
      s.stack.add newIntLitNode(b)
    tOct      <- oct:
      var o: BiggestInt
      assert len($0) == parseOct[BiggestInt]($0, o)
      s.stack.add newIntLitNode(o)
    tDec      <- dec:
      var d: BiggestInt
      assert len($0) == parseBiggestInt($0, d)
      s.stack.add newIntLitNode(d)
    tHex      <- hex:
      var x: BiggestInt
      assert len($0) == parseHex[BiggestInt]($0, x)
      s.stack.add newIntLitNode(x)
    tFloat    <- >(int * '.' * int * ?('e' * ?{'+', '-'} * int)) * S:
      var f: BiggestFloat
      assert len($1) == parseBiggestFloat($1, f)
      s.stack.add newFloatLitNode(f)
    tBool     <- >("true" | "false") * S:
      s.stack.add newLit(parseBool($1))
    tStr      <- '\"' * *(Print - '\"') * '\"' * S:
      s.stack.add newStrLitNode(($0)[1..^2])
    tMeth     <- >("to_s" | "to_i" | "length" | "reverse" | "substring" |
                 "first" | "last" | "size" | "min" | "max") * S:
      s.stack.add ident($1)
    tId       <- >((Lower | '_') * *(Alnum | '_')) * S:
      if s.context != "": s.stack.add newDotExpr(ident(s.context), ident($1))
      else: s.stack.add ident($1)

    # Aux
    int       <- hex | oct | bin | dec
    bin       <- "0b" * +{'0', '1', '_'}
    oct       <- "0o" * +{'0' .. '7', '_'}
    dec       <- Digit * *(?'_' * Digit)
    hex       <- "0x" * +(Xdigit | '_')
    S <- *Space

  if not p.match(txt, s).ok or s.stack.len != 1:
    raise newException(ParsingError, "Failed to parse expression: " & txt)
  s.stack[0]

proc nativeType*(ksyType: string): NimNode =
  case ksyType
  of "b1": result = ident"bool"
  of "u1": result = ident"uint8"
  of "s1": result = ident"int8"
  of "u2", "u2le", "u2be": result = ident"uint16"
  of "s2", "s2le", "s2be": result = ident"int16"
  of "u4", "u4le", "u4be": result = ident"uint32"
  of "s4", "s4le", "s4be": result = ident"int32"
  of "u8", "u8le", "u8be": result = ident"uint64"
  of "s8", "s8le", "s8be": result = ident"int64"
  of "f4", "f4be", "f4le": result = ident"float32"
  of "f8", "f8be", "f8le": result = ident"float64"
  of "str", "strz": result = ident"string"
  elif ksyType.match(re"b[2-9]|b[1-9][0-9]*"):
    result = ident"uint64"
  else:
    # TODO: implement look-up here
    result = ident(ksyType.capitalizeAscii)

proc debug(s: string) =
  let expr = expr(s, "this")
  echo treeRepr expr
  echo ""
  echo repr expr
  echo ""

#static: debug"a.b + e.f.g * h + 3"

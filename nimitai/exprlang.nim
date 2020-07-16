# Kaitai Struct Expression Language

import macros, npeg, parseutils

proc expr(e: string): NimNode =
  let parser = peg("G", expr: NimNode):
    G          <- integer
    integer    <- hex | oct | bin | dec
    hex        <- "0x" * +(Xdigit | '_'):
      var x: BiggestUInt
      assert len($0) == parseHex[BiggestUInt]($0, x)
      expr = newLit(x)
    oct        <- "0o" * +{'0' .. '7', '_'}:
      var x: BiggestUInt
      assert len($0) == parseOct[BiggestUInt]($0, x)
      expr = newLit(x)
    bin        <- "0b" * +{'0', '1', '_'}:
      var x: BiggestUInt
      assert len($0) == parseBin[BiggestUInt]($0, x)
      expr = newLit(x)
    dec        <- +(Digit | '_'):
      var x: BiggestUInt
      assert len($0) == parseBiggestUInt($0, x)
      expr = newLit(x)

  doAssert parser.match(e, result).ok

static: echo repr expr("0x12")

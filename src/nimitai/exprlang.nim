# Kaitai Struct Expression Language

import parseutils, macros, strformat
import strutils except parseBiggestInt
import npeg

type
  KsNodeKind = enum
    knkIdx
    knkArr
    knkInfix
    knkMethod
    knkBool
    knkInt
    knkFloat
    knkStr
    knkId
    knkOp
  KsNode = ref object
    case kind: KsNodeKind
    of knkBool:
      boolval: bool
    of knkInt:
      intval: BiggestInt
    of knkFloat:
      floatval: float
    of knkStr, knkId, knkOp:
      strval: string
    else:
      sons: seq[KsNode]
  ParsingError* = object of CatchableError

proc add(node: KsNode, children: varargs[KsNode]) =
  for c in children:
    node.sons.add(c)

proc newKsNode(kind: KsNodeKind, children: varargs[KsNode]): KsNode =
  doAssert kind in {knkIdx, knkArr, knkInfix, knkMethod}
  result = KsNode(kind: kind)
  for c in children:
    result.add(c)

proc `[]`(node: KsNode, index: int): KsNode =
  doAssert node.kind in {knkIdx, knkArr, knkInfix, knkMethod}
  node.sons[index]

proc toKs(str: string): KsNode =
  let p = peg(G, s: seq[seq[KsNode]]):
    # Non-terminal
    G         <- S * expr * !1
    expr      <- S * prefix * *infix
    prefix    <- tBool | tFloat | tInt | tStr | tId | idx | arr | parExpr
    idx       <- tId * arrOpen * expr * arrClose:
      let (idx, id) = (pop(s[^1]), pop(s[^1]))
      s[^1].add newKsNode(knkIdx, id, idx)
    arr       <- arrOpen * newLvl * expr * *(',' * expr) * arrClose:
      let
        elements = pop(s)
        newNode = newKsNode(knkArr)
      for e in elements:
        newNode.add(e)
      s[^1].add newNode
    parExpr   <- (parOpen * expr * parClose) ^ 0
    infix     <- >("not")                                 * expr ^  1 |
                 >("or" | "^")                            * expr ^  2 |
                 >("and")                                 * expr ^  3 |
                 >(">=" | ">" | "<=" | "<" | "==" | "!=") * expr ^  4 |
                 >("<<" | ">>" | "&" | "|")               * expr ^  5 |
                 >("+" | "-")                             * expr ^  6 |
                 >("*" | "/")                             * expr ^  7 |
                 >("%")                                   * expr ^  8 |
                 >(".")                                   * expr ^  9:
      var (r, l) = (pop(s[^1]), pop(s[^1]))
      case $1
      of ".":
        s[^1].add newKsNode(knkMethod, l, r)
      else:
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
    tStr      <- '\"' * >*(Print - '\"') * '\"' * S:
      s[^1].add KsNode(kind: knkStr, strval: $1)
    tId       <- >((Lower | '_') * *(Alnum | '_')) * S:
      s[^1].add KsNode(kind: knkId, strval: $1)

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

proc removeLeadingUnderscores(s: var string) =
  while s[0] == '_':
    s.delete(0, 0)

proc fixIds(node: var KsNode) =
  if node.kind in {knkIdx, knkArr, knkInfix, knkMethod}:
    for i in 0 ..< node.sons.len:
      fixIds(node.sons[i])
  elif node.kind == knkId:
    removeLeadingUnderscores(node.strval)

proc addContext(node: var KsNode, context: string) =
  node = newKsNode(
    knkMethod,
    KsNode(kind: knkId, strval: context),
    KsNode(kind: knkId, strval: node.strval))

proc addContextRec(node: var KsNode, context: string) =
  if node.kind in {knkIdx, knkArr, knkInfix}:
    for i in 0 ..< node.sons.len:
      if node.sons[i].kind == knkId:
        node.sons[i].addContext(context)
      else:
        node.sons[i].addContextRec(context)
  if node.kind == knkMethod:
    if node.sons[0].kind == knkId:
      node.sons[0].addContext(context)
    else:
      node.sons[0].addContextRec(context)

proc toNim(ks: KsNode): NimNode =
  case ks.kind
  of knkIdx:
    result = nnkBracketExpr.newTree(ks[0].toNim, ks[1].toNim)
  of knkArr:
    let x = newTree(nnkBracket)
    for s in ks.sons:
      x.add s.toNim
    result = prefix(x, "@")
  of knkInfix:
    result = nnkInfix.newTree(ks[1].toNim, ks[0].toNim, ks[2].toNim)
  of knkBool:
    result = newLit(ks.boolval)
  of knkInt:
    result = newIntLitNode(ks.intval)
  of knkFloat:
    result = newFloatLitNode(ks.floatval)
  of knkStr:
    result = newStrLitNode(ks.strval)
  of knkId:
    result = ident(ks.strval)
  of knkMethod:
    result = newDotExpr(ks[0].toNim, ks[1].toNim)
  of knkOp:
    var op: string
    case ks.strval
    of "%" : op = "mod"
    of "<<": op = "shl"
    of ">>": op = "shr"
    of "&" : op = "and"
    of "|" : op = "or"
    of "^" : op = "xor"
    else   : op = ks.strval
    result = ident(op)

proc ksAsStrToNim*(str, context: string): NimNode =
  var ks = str.toKs
  fixIds(ks)
  if context != "":
    ks.addContextRec(context)
    if ks.kind == knkId:
      ks.addContext(context)
  ks.toNim

proc debug(s: string) =
  let expr = ksAsStrToNim(s, "result")
  echo treeRepr expr
  echo ""
  echo repr expr
  echo ""

#static: debug"[0xff]"

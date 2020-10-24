# Kaitai Struct Expression Language

import parseutils, macros, strformat
import strutils except parseBiggestInt
import npeg, regex

type
  KsNodeKind = enum
    knkIdx
    knkArr
    knkMethod
    knkTernary
    knkInfix
    knkUnary
    knkEnum
    knkCast
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

proc isPrim*(ksType: string): bool =
  ksType.match(re"[su][1248](be|le)?|f[48]|b[1-9][0-9]*(be|le)?|strz?")

proc toPrim*(ksType: string): string =
  case ksType
  of "b1", "b1be", "b1le": result = "bool"
  of "u1": result = "uint8"
  of "s1": result = "int8"
  of "u2", "u2le", "u2be": result = "uint16"
  of "s2", "s2le", "s2be": result = "int16"
  of "u4", "u4le", "u4be": result = "uint32"
  of "s4", "s4le", "s4be": result = "int32"
  of "u8", "u8le", "u8be": result = "uint64"
  of "s8", "s8le", "s8be": result = "int64"
  of "f4", "f4be", "f4le": result = "float32"
  of "f8", "f8be", "f8le": result = "float64"
  of "str", "strz": result = "string"
  elif ksType.match(re"b[2-9]|b[1-9][0-9]*(be|le)?"):
    result = "uint64"
  else: discard # should not occure

proc isFatherKind(kind: KsNodeKind): bool =
  kind in {knkIdx, knkArr, knkMethod, knkTernary, knkInfix, knkUnary, knkEnum}

proc add(node: KsNode, children: varargs[KsNode]) =
  for c in children:
    node.sons.add(c)

proc newKsNode(kind: KsNodeKind, children: varargs[KsNode]): KsNode =
  doAssert isFatherKind(kind)
  result = KsNode(kind: kind)
  for c in children:
    result.add(c)

proc `[]`(node: KsNode, index: int): KsNode =
  doAssert isFatherKind(node.kind)
  node.sons[index]

proc scopedId(s: string): string = replace(s, "::").capitalizeAscii

proc transpileType(s: string): string =
  result = if s.isPrim: s.toPrim
           else: scopedId(s)

proc toKs(str: string): KsNode =
  let p = peg(G, s: seq[seq[KsNode]]):
    # Non-terminal
    G         <- S * expr * !1
    expr      <- S * prefix * *infix
    prefix    <- idx | arr | unary | parExpr |
                 tBool | tFloat | tInt | tStr | tCast | tEnum | tId
    unary     <- >{'+','-'} * expr:
      s[^1].add newKsNode(knkUnary, KsNode(kind: knkOp, strval: $1), pop(s[^1]))
    idx       <- >id * S * arrOpen * expr * arrClose:
      s[^1].add newKsNode(knkIdx, KsNode(kind: knkId, strval: $1), pop(s[^1]))
    arr       <- arrOpen * newLvl * *(expr * *(',' * expr)) * arrClose:
      let
        elements = pop(s)
        newNode = newKsNode(knkArr)
      for e in elements:
        newNode.add(e)
      s[^1].add newNode
    parExpr   <- (parOpen * expr * parClose) ^ 0
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
        s[^1].add newKsNode(knkMethod, l, r)
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
    tStr      <- ('\"' * >*(Print - '\"') * '\"') |
                 ('\'' * >*(Print - '\'') * '\'') * S:
      s[^1].add KsNode(kind: knkStr, strval: $1)
    tCast     <- "as<" * S * >(id * *("::" * id)) * S * '>' * S:
      s[^1].add KsNode(kind: knkId, strval: transpileType($1))
    tEnum     <- >(id * "::" * id * *("::" * id)) * S:
      let split = rsplit($1, "::", maxsplit=1)
      let
        scope = scopedId(split[0])
        value = split[1]
      s[^1].add newKsNode(
        knkEnum,
        KsNode(kind: knkId, strval: scope),
        KsNode(kind: knkId, strval: value))
    tId       <- >id * S:
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

proc toNim(ks: KsNode): NimNode =
  case ks.kind
  of knkIdx:
    result = nnkBracketExpr.newTree(ks[0].toNim, ks[1].toNim)
  of knkArr:
    let x = newTree(nnkBracket)
    for s in ks.sons:
      x.add s.toNim
    result = prefix(x, "@")
  of knkMethod, knkEnum:
    result = newDotExpr(ks[0].toNim, ks[1].toNim)
  of knkTernary:
    result = nnkIfStmt.newTree(
      nnkElifBranch.newTree(
        ks[0].toNim,
        ks[1].toNim),
      nnkElse.newTree(
        ks[2].toNim))
  of knkInfix:
    result = nnkInfix.newTree(ks[1].toNim, ks[0].toNim, ks[2].toNim)
  of knkUnary:
    result = nnkPrefix.newTree(ks[0].toNim, ks[1].toNim)
  of knkBool:
    result = newLit(ks.boolval)
  of knkInt:
    result = newIntLitNode(ks.intval)
  of knkFloat:
    result = newFloatLitNode(ks.floatval)
  of knkStr:
    result = newStrLitNode(ks.strval)
  of knkId, knkCast:
    result = ident(ks.strval)
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

proc removeLeadingUnderscores(s: var string) =
  while s[0] == '_':
    s.delete(0, 0)

proc fixIds(node: var KsNode) =
  if isFatherKind(node.kind):
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
  if node.kind in {knkIdx, knkArr, knkTernary, knkInfix, knkUnary}:
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

#static: debug"a ? b : c"

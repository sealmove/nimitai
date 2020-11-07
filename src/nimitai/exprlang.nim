# Kaitai Struct Expression Language

import parseutils, macros, strformat, json
import strutils except parseBiggestInt
import npeg, regex
import types

type
  State = object
    ps: seq[seq[KsNode]]
    typ: Type
  ParsingError* = object of CatchableError

proc debug*(ks: KsNode, n = 0) =
  let k = " ".repeat(n) & ($ks.kind)[3..^1]
  case ks.kind
  of knkCast:
    echo k & " " & $ks.kstype.kind
  of knkEnum:
    echo k & " " & ks.enumscope.join("::") & ", " & ks.enumval
  of knkBool:
    echo k & " " & $ks.boolval
  of knkInt:
    echo k & " " & $ks.intval
  of knkFloat:
    echo k & " " & $ks.floatval
  of knkStr, knkId, knkOp:
    echo k & " " & $ks.strval
  else:
    echo k
    for s in ks.sons:
      debug(s, n + 2)

proc isFatherKind(kind: KsNodeKind): bool =
  kind in {knkArr, knkMeth, knkIdx, knkDotExpr, knkUnary, knkInfix, knkTernary}

proc add(node: KsNode, children: varargs[KsNode]) =
  for c in children:
    node.sons.add(c)

proc newKsNode*(kind: KsNodeKind, cx: Type, children: varargs[KsNode]): KsNode =
  doAssert isFatherKind(kind)
  result = KsNode(kind: kind, cx: cx)
  for c in children:
    result.add(c)

proc `[]`(node: KsNode, index: int): KsNode =
  doAssert isFatherKind(node.kind)
  node.sons[index]

proc parseType*(s: string, typ: Type): KsType =
  if s.match(re"u[1248](be|le)?"):
    result = tuint(parseInt(s[1..1]))
    if s.match(re".*(be|le)"):
      result.endian = parseEnum[Endian](s[2..3])
  elif s.match(re"s[1248](be|le)?"):
    result = tsint(parseInt(s[1..1]))
    if s.match(re".*(be|le)"):
      result.endian = parseEnum[Endian](s[2..3])
  elif s.match(re"f[48](be|le)?"):
    result = tfloat(parseInt(s[1..1]))
    if s.match(re".*(be|le)"):
      result.endian = parseEnum[Endian](s[2..3])
  elif s.match(re"b[1-9][0-9]*(be|le)?"):
    if s.match(re".*(be|le)"):
      result = tbit(parseInt(s[1..^3]), parseEnum[Endian](s[^2..^1]))
    else:
      result = tbit(parseInt(s[1..^1]))
  elif s.match(re"strz?"):
    result = tstr(if s.endsWith('z'): true else: false)
  else:
    var
      scope = split(s, "::")
      path: seq[string]
    let name = scope[0]
    for i in countdown(scope.len - 1, 1):
      path.add(scope[i])
    result = tuser(typ, name, path)

proc toKs*(str: string, typ: Type): KsNode =
  let p = peg(G, s: State):
    # Non-terminal
    G         <- S * expr * !1
    expr      <- S * prefix * *infix
    prefix    <- arr | meth | idx | unary | parExpr |
                 tBool | tFloat | tInt | tStr | tCast | tEnum | tId
    arr       <- '[' * S * newLvl * *(expr * *(',' * expr)) * S * ']' * S:
      let
        elements = pop(s.ps)
        newNode = newKsNode(knkArr, s.typ)
      for e in elements:
        newNode.add(e)
      s.ps[^1].add newNode
    meth      <- >("to_s" | "to_i" | "length" | "substring" | "size" | "first" |
                   "last" | "min" | "max" | "reverse" | "eof") *
                 &(!(Alpha | '_' | '[')) * newLvl * ?('(' * expr *
                 *(',' * expr) * ')') * S:
      let
        elements = pop(s.ps)
        newNode = newKsNode(knkMeth, s.typ, KsNode(kind: knkId, strval: $1, cx: s.typ))
      for e in elements:
        newNode.add(e)
      s.ps[^1].add newNode
    idx       <- >id * S * '[' * expr * ']' * S:
      s.ps[^1].add newKsNode(knkIdx, s.typ, KsNode(kind: knkId, strval: $1, cx: s.typ), pop(s.ps[^1]))
    unary     <- (>("+"|"-"|"not") * S * expr * S) ^ 9:
      s.ps[^1].add newKsNode(knkUnary, s.typ, KsNode(kind: knkOp, strval: $1, cx: s.typ), pop(s.ps[^1]))
    parExpr   <- ('(' * expr * ')' * S) ^ 0
    infix     <- >("?") * S * expr ^ 1 * ":" * S          * expr      |
                 >("or" | "^")                            * expr ^  2 |
                 >("and")                                 * expr ^  3 |
                 >(">=" | ">" | "<=" | "<" | "==" | "!=") * expr ^  4 |
                 >("<<" | ">>" | "&" | "|")               * expr ^  5 |
                 >("+" | "-")                             * expr ^  6 |
                 >("*" | "/")                             * expr ^  7 |
                 >("%")                                   * expr ^  8 |
                 >(".")                                   * expr ^ 10:
      case $1
      of ".":
        let (r, l) = (pop(s.ps[^1]), pop(s.ps[^1]))
        s.ps[^1].add newKsNode(knkDotExpr, s.typ, l, r)
      of "?":
        let (second, first, condition) = (pop(s.ps[^1]), pop(s.ps[^1]), pop(s.ps[^1]))
        s.ps[^1].add newKsNode(knkTernary, s.typ, condition, first, second)
      else:
        let (r, l) = (pop(s.ps[^1]), pop(s.ps[^1]))
        s.ps[^1].add newKsNode(knkInfix, s.typ, l, KsNode(kind: knkOp, strval: $1, cx: s.typ), r)

    # Terminal
    tBool     <- >("true" | "false") * S:
      s.ps[^1].add KsNode(kind: knkBool, boolval: parseBool($1), cx: s.typ)
    tFloat    <- >(int * '.' * int * ?('e' * ?{'+', '-'} * int)) * S:
      var f: BiggestFloat
      assert len($1) == parseBiggestFloat($1, f)
      s.ps[^1].add KsNode(kind: knkFloat, floatval: f, cx: s.typ)
    tInt      <- (tBin | tOct | tHex | tDec) * S
    tBin      <- bin:
      var b: BiggestInt
      assert len($0) == parseBin[BiggestInt]($0, b)
      s.ps[^1].add KsNode(kind: knkInt, intval: b, cx: s.typ)
    tOct      <- oct:
      var o: BiggestInt
      assert len($0) == parseOct[BiggestInt]($0, o)
      s.ps[^1].add KsNode(kind: knkInt, intval: o, cx: s.typ)
    tDec      <- dec:
      var d: BiggestInt
      assert len($0) == parseBiggestInt($0, d)
      s.ps[^1].add KsNode(kind: knkInt, intval: d, cx: s.typ)
    tHex      <- hex:
      var x: BiggestInt
      assert len($0) == parseHex[BiggestInt]($0, x)
      s.ps[^1].add KsNode(kind: knkInt, intval: x, cx: s.typ)
    tStr      <- (('\"' * >*(Print - '\"') * '\"') |
                  ('\'' * >*(Print - '\'') * '\'')) * S:
      s.ps[^1].add KsNode(kind: knkStr, strval: $1, cx: s.typ)
    tId       <- >id * S:
      s.ps[^1].add KsNode(kind: knkId, strval: $1, cx: s.typ)
    tEnum     <- >enumer * S:
      let x = split($1, "::")
      s.ps[^1].add KsNode(kind: knkEnum, enumscope: x[0..^2], enumval: x[^1], cx: s.typ)
    tCast     <- "as<" * S * >typ * S * '>' * S:
      s.ps[^1].add KsNode(kind: knkCast, kstype: parseType($1, s.typ), cx: s.typ)

    # Aux
    S        <- *Space
    int      <- hex | oct | bin | dec
    bin      <- "0b" * +{'0', '1', '_'}
    oct      <- "0o" * +{'0' .. '7', '_'}
    dec      <- Digit * *(?'_' * Digit)
    hex      <- "0x" * +(Xdigit | '_')
    id       <- (Lower | '_') * *(Alnum | '_')
    enumer   <- id * "::" * id * *("::" * id)
    prim     <- {'u','s'} * {'1','2','4','8'} * ?("be"|"le") |
                'f' * {'4','8'} * ?("be"|"le") |
                'b' * {'1'..'9'} * *Digit |
                "str" * ?'z'
    typ      <- prim | enumer | id

    # Aux with side-effects
    newLvl   <- 0:
      s.ps.add newSeq[KsNode]()

  var s = State(typ: typ)
  s.ps.add newSeq[KsNode]()
  if not p.match(str, s).ok:
    raise newException(ParsingError, str)
  elif s.ps.len == 0:
    raise newException(ParsingError, str & &" (stacks: {s.ps.len})")
  elif s.ps[^1].len != 1:
    raise newException(ParsingError, str & &" (items: {s.ps[0].len})")
  result = s.ps[^1][0]
  #debug(result)

proc parseType*(json: JsonNode, typ: Type): KsType =
  case json.kind
  of JString: result = parseType(json.getStr, typ)
  of JObject:
    let on = json["switch-on"].getStr.toKs(typ)
    result = KsType(kind: ktkSwitch, on: on)
    for k, v in json["cases"]:
      result.cases.add((k.toKs(typ), parseType(v, typ)))
  else: discard # should not occur

#static: discard "[0x42, 0x1337, -251658241, -1]".toKs(nil)

# Kaitai Struct Expression Language

import parseutils, sequtils, macros, regex
import strutils except parseBiggestInt
import npeg

type
  TokenKind = enum
    tkFloat
    tkInteger
    tkBoolean
    tkString
    tkComma
    tkParenOpen
    tkParenClose
    tkArrayOpen
    tkArrayClose
    tkOp
    tkDot
    tkMethod
    tkId
  Token = ref object
    case kind: TokenKind
    of tkFloat:
      floatVal: BiggestFloat
    of tkInteger:
      intVal: BiggestUInt
    of tkBoolean:
      boolVal: bool
    of tkString, tkOp, tkMethod, tkId:
      strVal: string
    else: discard
  State = ref object
    stack: seq[NimNode]
    stackCnt: int
    context: string
  LexingError* = object of CatchableError
  ParsingError* = object of CatchableError

proc `==`(token: Token, tk: TokenKind): bool =
  token.kind == tk

proc tokenize(str: string): seq[Token] =
  var tokens: seq[Token]
  let l = peg G:
    B <- *Blank
    G <- *((Float | Integer | Boolean | String | Comma | ParenOpen |
          ParenClose | ArrayOpen | ArrayClose | Op | Dot | Method | Id) * B) *
          !1
    Float      <- Int * '.' * Int * ?('e' * ?{'+', '-'} * Int):
      var x: BiggestFloat
      assert len($0) == parseBiggestFloat($0, x)
      tokens.add Token(kind: tkFloat, floatVal: x)
    Int     <- Hex | Oct | Bin | Dec
    Integer <- HexR | OctR | BinR | DecR
    Hex     <- "0x" * +(Xdigit | '_')
    Oct     <- "0o" * +{'0' .. '7', '_'}
    Bin     <- "0b" * +{'0', '1', '_'}
    Dec     <- +(Digit | '_')
    HexR    <- Hex:
      var x: BiggestUInt
      assert len($0) == parseHex[BiggestUInt]($0, x)
      tokens.add Token(kind: tkInteger, intVal: x)
    OctR     <- Oct:
      var x: BiggestUInt
      assert len($0) == parseOct[BiggestUInt]($0, x)
      tokens.add Token(kind: tkInteger, intVal: x)
    BinR     <- Bin:
      var x: BiggestUInt
      assert len($0) == parseBin[BiggestUInt]($0, x)
      tokens.add Token(kind: tkInteger, intVal: x)
    DecR     <- Dec:
      var x: BiggestUInt
      #assert len($0) == parseBiggestUInt($0, x)
      tokens.add Token(kind: tkInteger, intVal: x)
    Boolean    <- "true" | "false":
      tokens.add Token(kind: tkBoolean, boolval: parseBool($0))
    String     <- '\"' * *(Print - '\"') * '\"':
      tokens.add Token(kind: tkString, strval: ($0)[1..^2])
    Comma      <- ',':
      tokens.add Token(kind: tkComma)
    ParenOpen  <- '(':
      tokens.add Token(kind: tkParenOpen)
    ParenClose <- ')':
      tokens.add Token(kind: tkParenClose)
    ArrayOpen  <- '[':
      tokens.add Token(kind: tkArrayOpen)
    ArrayClose <- ']':
      tokens.add Token(kind: tkArrayClose)
    Op         <- "+" | "-"  | "*" | "/"  | "%" | "<<" | ">>" |  "&"  | "|"  |
                  "^" | ">=" | ">" | "<=" | "<" | "==" | "!=" | "and" | "or" |
                  "::":
      var op: string
      case $0
      of "%" : op = "mod"
      of "<<": op = "shl"
      of ">>": op = "shr"
      of "&" : op = "and"
      of "|" : op = "or"
      of "^" : op = "xor"
      else   : op = $0
      tokens.add Token(kind: tkOp, strVal: op)
    Dot        <- ".":
      tokens.add Token(kind: tkDot)
    Method     <- "to_s" | "to_i" | "length" | "reverse" | "substring" |
                  "first" | "last" | "size" | "min" | "max":
      tokens.add Token(kind: tkMethod, strVal: $0)
    Id         <- (Lower | '_') * *(Alnum | '_'):
      tokens.add Token(kind: tkId, strVal: $0)

  if not l.match(str).ok:
    raise newException(LexingError, "Could not tokenize")
  tokens

proc parse(tokens: seq[Token], context: string): NimNode =
  var s = State(context: context)
  let p = peg(G, Token, s: State):
    CS <- 0:
      s.stackCnt = s.stack.len
    G <- Expr * !1
    Expr  <- Term  * *Infix
    Expr2 <- Term2 * *Infix
    Infix <- (>[tkOp]  * Expr)  ^ 1 |
             (>[tkDot] * Expr2) ^ 2:
      let
        right = s.stack.pop
        left = s.stack.pop
      case ($1).kind
      of tkDot: s.stack.add newDotExpr(left, right)
      else: s.stack.add infix(left, ($1).strval, right)
    Term  <- ParenExpr | Array | Float | Integer | Boolean | String | Method |
             Id | Prefix
    Term2 <- ParenExpr | Array | Float | Integer | Boolean | String | Method |
             Id2 | Prefix
    Prefix <- >[tkOp] * Term:
      s.stack.add prefix(s.stack.pop, ($1).strval)
    ParenExpr <- [tkParenOpen] * Expr * [tkParenClose]
    Array <- [tkArrayOpen] * CS * Expr * *([tkComma] * Expr) *
             [tkArrayClose]:
      let
        b = s.stackCnt
        f = s.stack.len - 1
      var contents = newTree(nnkBracket)
      for i in b .. f:
        contents.add(s.stack[i])
      s.stack.setlen(b)
      s.stack.add prefix(contents, "@")
    Float <- [tkFloat]:
      s.stack.add newLit(($0).floatVal)
    Integer <- [tkInteger]:
      s.stack.add newLit(($0).intVal)
    Boolean <- [tkBoolean]:
      s.stack.add newLit(($0).boolVal)
    String <- [tkString]:
      s.stack.add newLit(($0).strVal)
    Method <- [tkMethod]:
      s.stack.add ident(($0).strVal)
    Id  <- [tkId]:
      if s.context != "":
        s.stack.add newDotExpr(ident(s.context), ident(($0).strVal))
      else:
        s.stack.add ident(($0).strVal)
    Id2 <- [tkId]:
      s.stack.add ident(($0).strVal)

  if not p.match(tokens, s).ok:
    raise newException(ParsingError, "Could not parse")
  if not s.stack.len == 1:
    raise newException(ParsingError, "Got leftover nodes")

  s.stack[0]

proc expr*(s: string, context: string): NimNode = parse(s.tokenize, context)

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
  let tokens = tokenize(s)
  echo "=== TOKENS ==="
  for t in tokens:
    case t.kind
    of tkFloat:
      echo "Float(" & $t.floatVal & ")"
    of tkInteger:
      echo "Integer(" & $t.intVal & ")"
    of tkBoolean:
      echo "Boolean(" & $t.boolVal & ")"
    of tkString:
      echo "String(" & t.strVal & ")"
    of tkOp:
      echo "Op(" & t.strVal & ")"
    of tkId:
      echo "Id(" & t.strVal & ")"
    else:
      let s = $t.kind
      echo s[2 .. ^1]
  echo ""
  let expr = parse(tokens, "this")
  echo "=== EXPRESSION ==="
  echo treeRepr expr
  echo ""
  echo repr expr
  echo ""

#static: debug"a.b.c.d + e.f.g"

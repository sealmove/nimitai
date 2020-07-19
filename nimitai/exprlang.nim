# Kaitai Struct Expression Language

import parseutils, sequtils, macros
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
    tkId
  Token = ref object
    case kind: TokenKind
    of tkFloat:
      floatVal: BiggestFloat
    of tkInteger:
      intVal: BiggestUInt
    of tkBoolean:
      boolVal: bool
    of tkString, tkOp, tkId:
      strVal: string
    else: discard
  State = ref object
    stack: seq[NimNode]
    stackCnt: int

proc `==`(token: Token, tk: TokenKind): bool =
  token.kind == tk

proc tokenize(str: string): seq[Token] =
  var tokens: seq[Token]
  let l = peg G:
    B <- *Blank
    G <- *((Float | Integer | Boolean | String | Comma | ParenOpen |
            ParenClose | ArrayOpen | ArrayClose | Op | Id) * B) * !1
    Float      <- Int * '.' * Int * ?('e' * ?{'+', '-'} * Int):
      var x: BiggestFloat
      assert len($0) == parseBiggestFloat($0, x)
      tokens.add Token(kind: tkFloat, floatVal: x)
    Int        <- Hex | Oct | Bin | Dec
    Integer    <- HexR | OctR | BinR | DecR
    Hex        <- "0x" * +(Xdigit | '_')
    Oct        <- "0o" * +{'0' .. '7', '_'}
    Bin        <- "0b" * +{'0', '1', '_'}
    Dec        <- +(Digit | '_')
    HexR     <- Hex:
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
      assert len($0) == parseBiggestUInt($0, x)
      tokens.add Token(kind: tkInteger, intVal: x)
    Boolean    <- "true" | "false":
      tokens.add Token(kind: tkBoolean, boolval: parseBool($0))
    String     <- '\"' * >*(Print - '\"') * '\"':
      tokens.add Token(kind: tkString, strval: $0)
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
                  "." | "::":
      tokens.add Token(kind: tkOp, strVal: $0)
    Id         <- (Lower | '_') * *(Alnum | '_'):
      tokens.add Token(kind: tkId, strVal: $0)

  assert l.match(str).ok
  tokens

proc parse(tokens: seq[Token]): NimNode =
  var s = State()
  let p = peg(G, Token, s: State):
    CS <- 0:
      s.stackCnt = s.stack.len
    G <- Expr * !1
    Expr <- Term * *Infix
    Infix <- >[tkOp] * Term:
      s.stack.add infix(s.stack.pop, ($1).strval, s.stack.pop)
    Term <- ParenExpr | Array | Float | Integer | Boolean | String | Id | Prefix
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
    Id <- [tkId]:
      s.stack.add ident(($0).strVal)

  assert p.match(tokens, s).ok
  assert s.stack.len == 1
  s.stack[0]

proc expr*(s: string): NimNode = s.tokenize.parse

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
  let expr = parse(tokens)
  echo "=== EXPRESSION ==="
  echo repr expr
  echo ""

#static: debug("3.5 + 3")

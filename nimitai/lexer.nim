import strutils, sequtils, npeg

type
  Token* = tuple[kind: TokenKind, value: string]
  TokenKind* = enum
    tkMeta
    tkSeq
    tkTypes
    tkInstances
    tkEnums
    tkDoc
    tkDocRef
    tkApplication
    tkConsume
    tkContents
    tkEncoding
    tkEndian
    tkEnum
    tkEosError
    tkFileExtension
    tkId
    tkIf
    tkImports
    tkInclude
    tkIo
    tkKsDebug
    tkKsOpaqueTypes
    tkKsVersion
    tkLicense
    tkPadRight
    tkProcess
    tkPos
    tkRepeat
    tkRepeatExpr
    tkRepeatUntil
    tkSize
    tkSizeEos
    tkTerminator
    tkTitle
    tkType
    tkValue
    tkName
    tkDash
    tkDocMark
    tkIndent
    tkDedent
    tkItem
    tkApostrophe

const keys = [
  "meta"            ,
  "seq"             ,
  "types"           ,
  "instances"       ,
  "enums"           ,
  "seq"             ,
  "types"           ,
  "instances"       ,
  "enums"           ,
  "application"     ,
  "consume"         ,
  "contents"        ,
  "doc"             ,
  "doc-ref"         ,
  "encoding"        ,
  "endian"          ,
  "enum"            ,
  "eos-error"       ,
  "file-extension"  ,
  "id"              ,
  "if"              ,
  "imports"         ,
  "include"         ,
  "io"              ,
  "ks-debug"        ,
  "ks-opaque-types" ,
  "ks-version"      ,
  "license"         ,
  "pad-right"       ,
  "process"         ,
  "pos"             ,
  "repeat"          ,
  "repeat-expr"     ,
  "repeat-until"    ,
  "size"            ,
  "size-eos"        ,
  "terminator"      ,
  "title"           ,
  "type"            ,
  "value"
]

proc tokenizeKsy*(path: string): seq[Token] =
  var
    indents: seq[int]
    tokens: seq[Token]
  indents.add(0)

  let lexer = peg G:
    k(item) <- item * B * ':' * B
    B <- *Blank
    G <- +((Dent | Key | Dash | DocMark | Apostrophe | Item) * B) * !1
    Dent <- +'\n' * >*' ':
      let s = len($1)
      var idx = indents.len - 1
      if s > indents[idx]:
        indents.add s
        tokens.add (tkIndent, $1)
      else:
        while indents[idx] != s:
          let i = pop(indents)
          dec idx
          tokens.add (tkDedent, ' '.repeat(i))
    Key <- k(>+(Alnum | '_' | '-')):
      if $1 in keys:
        tokens.add (parseEnum[TokenKind]("tk" & replace($1, "-")), "")
      else:
        tokens.add (tkName, $1)
    Dash <- '-':
      tokens.add (tkDash, $0)
    DocMark <- '|':
      tokens.add (tkDocMark, $0)
    Apostrophe <- '\'':
      tokens.add (tkApostrophe, $0)
    Item <- +(Print - '\n'):
      tokens.add (tkItem, $0)
      
  let file = readFile(path).splitLines
                           .filterIt(not it.strip.startsWith('#'))
                           .join("\n")
  assert lexer.match(file).ok
  tokens

proc `$`*(t: Token): string =
  let k = ($t.kind)[2..^1]
  result &= k
  if t.value.replace(" ") != "":
    result &= "(" & t.value & ")"

proc debugLexer*(path: string) =
  let tokens = tokenizeKsy(path)
  echo "=== TOKENS ==="
  for t in tokens:
    echo t
  echo ""

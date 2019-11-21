import npeg, ksyast, strutils

var stack = newSeq[KsyNode]()

proc newKey(kind: KeyKind, s: string): KsyNode =
  result = KsyNode(kind: knkKey)
  case kind:
  of kkConsume:
    result.keyNode = Key(kind: kind, consume: parseBool(s))
  else:
    discard
  #of kkContents:
  #  result.contents =
  #of kkEndian:
  #  result.endian =
  ##of kkEnum
  ##of kkEosError
  #of kkExts, kkImports:
  #  result.list =
  #of kkApp, kkEncoding, kkId, kkLicence, kkTitle, kkType:
  #  result.strval = s
  ##of kkIf
  ##of kkInclude
  ##of kkIo
  ##of kkProcess
  ##of kkPos
  #of kkRepeat:
  #  result.repeat =
  ##of kkRepeatExpr
  ##of kkRepeatUntil
  #of kkSize:
  #  result.size =
  ##of kkSizeEos
  ##of kkTerminator
  ##of kkValue:


#[XXX
  doc-ref Section
  KsyExpression <-
]#

let p = peg "ksy":
  # Templates
  K(item) <- item * Colon
  ArrayInline(item) <- '[' * B * item * B * *(',' * B * item * B) * ']'
  Array2(item) <- +(+'\n' * ' '[2] * item * B)
  Array4(item) <- +(+'\n' * ' '[4] * item * B)
  Array6(item) <- +(+'\n' * ' '[6] * item * B)
  Array8(item) <- +(+'\n' * ' '[8] * item * B)
  YamlArray2(item) <- +(+'\n' * ' '[2] * Tag * item * B)
  YamlArray4(item) <- +(+'\n' * ' '[4] * Tag * item * B)
  YamlArray6(item) <- +(+'\n' * ' '[6] * Tag * item * B)

  # Atoms
  B <- *Blank
  Any <- +(1 - '\n')
  Tag <- '-' * B
  Colon <- B * ':' * B
  Bool <- "true" | "false"
  String <- '"' * *(1 - {'"', '\n'}) * '"'
  Identifier <- {'a'..'z'} * *{'a'..'z','0'..'9','_'}
  Import <- +{'A'..'Z','a'..'z','0'..'9','_','-','/'}
  ArrayItem <- String | "0x" * +Xdigit | +Digit

  # Main grammar
  ksy <- +(>Section0 * +'\n') * !1
  Section0 <- Meta0 | Doc0 | Seq0 | Types | Instances0 | Enums0
  Section4 <- ' '[4] * (Meta4 | Doc4 | Seq4 | Instances4 | Enums4)
  Types <- K("types") * Array2(K(Identifier) * +(+'\n' * Section4))
  Meta0 <- K("meta") * Array2(>Key)
    #stack.add KsyNode(kind: knkSection, sectionNode: Section(kind: skMeta))
  Meta4 <- K("meta") * Array6(Key)
  Doc0 <- K("doc") * (('|' * B * *(+'\n' * ' '[2] * Any)) | Any)
  Doc4 <- K("doc") * (('|' * B * *(+'\n' * ' '[6] * Any)) | Any)
  Seq0 <- K("seq") * YamlArray2(K("id") * Identifier * Array4(Key))
  Seq4 <- K("seq") * YamlArray6(K("id") * Identifier * Array8(Key))
  Instances0 <- K("instances") * Array2(K(Identifier) * Array4(Key))
  Instances4 <- K("instances") * Array6(K(Identifier) * Array8(Key))
  Enums0 <- K("enums") * Array2(K(Identifier) *
            Array4(K(+Digit) * Identifier))
  Enums4 <- K("enums") * Array6(K(Identifier) *
            Array8(K(+Digit) * Identifier))
  Key <- App | Consume | Contents | Encoding | Endian | Enum | Enum | EosError |
         Ext | Id | If | Include | Imports | Io | License | Process | Pos |
         Repeat | RepeatExpr | RepeatUntil | Size | SizeEos | Terminator |
         Title | Type | Value

  # Keys
  App <- K("application") * >Any#:
    #stack.add KsyNode(kind: knkKey, keyNode: Key(kind: akApp, strval: $1))
  Consume <- K("consume") * >Bool * B:
    stack.add newKey(kkConsume, $1)
  Contents <- K("contents") *
              (ArrayItem | ArrayInline(ArrayItem) | YamlArray6(ArrayItem))
  Encoding <- K("encoding") * Any
  Endian <- K("endian") * ("le" | "be")
  Ext <- K("file-extension") * (YamlArray4(Any) | Any)
  Id <- K("id") * Identifier
  Imports <- K("imports") * (YamlArray4(Import) | Import)
  License <- K("license") * Any
  Repeat <- K("repeat") * ("expr" | "eos" | "until")
  Size <- K("size") * Any
  Title <- K("title") * Any
  Type <- K("type") * Identifier

  #XXX
  Enum <- K("enum") * Any
  EosError <- K("eos-error") * Any
  If <- K("if") * Any # Expression
  Include <- K("include") * Any
  Io <- K("io") * Any
  Process <- K("process") * Any
  Pos <- K("pos") * Any
  RepeatExpr <- K("repeat-expr") * Any # Expression
  RepeatUntil <- K("repeat-until") * Any # Expression
  SizeEos <- K("size-eos") * Any
  Terminator <- K("terminator") * Any
  Value <- K("value") * Any # Expression

doAssert p.matchFile("test.ksy").ok

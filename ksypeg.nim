import npeg, ksyast

#[XXX
  doc-ref Section
  KsyExpression <-
]#

let p = peg "ksy":
  # Templates
  Key(item) <- item * Colon
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
  ksy <- +(Section0 * +'\n') * !1
  Section0 <- >(Meta0 | Doc0 | Seq0 | Types | Instances0 | Enums0)
  Section4 <- ' '[4] * (Meta4 | Doc4 | Seq4 | Instances4 | Enums4)
  Types <- Key("types") * Array2(Key(Identifier) * +(+'\n' * Section4))
  Meta0 <- Key("meta") * Array2(MetaAttrs)
  Meta4 <- Key("meta") * Array6(MetaAttrs)
  Doc0 <- Key("doc") * (('|' * B * *(+'\n' * ' '[2] * Any)) | Any)
  Doc4 <- Key("doc") * (('|' * B * *(+'\n' * ' '[6] * Any)) | Any)
  Seq0 <- Key("seq") * YamlArray2(Key("id") * Identifier * Array4(Attr))
  Seq4 <- Key("seq") * YamlArray6(Key("id") * Identifier * Array8(Attr))
  Instances0 <- Key("instances") * Array2(Key(Identifier) * Array4(Attr))
  Instances4 <- Key("instances") * Array6(Key(Identifier) * Array8(Attr))
  Enums0 <- Key("enums") * Array2(Key(Identifier) *
            Array4(Key(+Digit) * Identifier))
  Enums4 <- Key("enums") * Array6(Key(Identifier) *
            Array8(Key(+Digit) * Identifier))
  Attr <- Consume | Contents | Encoding | Endian | Enum | Enum | EosError | If |
          Include | Io | Process | Pos | Repeat | RepeatExpr | RepeatUntil |
          Size | SizeEos | Terminator | Type | Value

  # Section Attributes
  MetaAttrs <- Id | Title | Application | Ext2 | License | Imports2 | Encoding |
                Endian

  # Attributes
  Application <- Key("application") * Any
  Consume <- Key("consume") * Bool
  Contents <- Key("contents") *
              (ArrayItem | ArrayInline(ArrayItem) | YamlArray6(ArrayItem))
  Encoding <- Key("encoding") * Any
  Endian <- Key("endian") * ("le" | "be")
  Ext2 <- Key("file-extension") * (YamlArray4(Any) | Any)
  Id <- Key("id") * Identifier
  Imports2 <- Key("imports") * (YamlArray4(Import) | Import)
  License <- Key("license") * Any
  Repeat <- Key("repeat") * ("expr" | "eos" | "until")
  Size <- Key("size") * Any
  Title <- Key("title") * Any
  Type <- Key("type") * Identifier

  #XXX
  Enum <- Key("enum") * Any
  EosError <- Key("eos-error") * Any
  If <- Key("if") * Any # Expression
  Include <- Key("include") * Any
  Io <- Key("io") * Any
  Process <- Key("process") * Any
  Pos <- Key("pos") * Any
  RepeatExpr <- Key("repeat-expr") * Any # Expression
  RepeatUntil <- Key("repeat-until") * Any # Expression
  SizeEos <- Key("size-eos") * Any
  Terminator <- Key("terminator") * Any
  Value <- Key("value") * Any # Expression

doAssert p.matchFile("test.ksy").ok

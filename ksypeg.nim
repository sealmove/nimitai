import npeg, strutils

#[ XXX
  doc-ref
]#

let p = peg "ksy":
  ksy <- +(Section0 * +'\n') * !1

  # Atoms
  B <- *Blank
  Any <- +(1 - '\n')
  Tag <- '-' * B
  Colon <- B * ':' * B
  Bool <- ("true" | "false") * B
  Identifier <- {'a'..'z'} * *{'a'..'z','0'..'9','_'}
  Import <- +{'A'..'Z','a'..'z','0'..'9','_','-','/'}
  Key(item) <- item * Colon

  Section0 <- >(Meta0 | Doc0 | Seq0 | Types0 | Instances0 | Enums0):
    echo $0

  Section4 <- ' '[4] * (Meta4 | Doc4 | Seq4 | Instances4 | Enums4)

  # Sections
  Meta0 <- Key("meta") * +(+'\n' * ' '[2] * MetaAttrs2)
  Doc0 <- Key("doc") * (('|' * B * *(+'\n' * ' '[2] * Any)) | Any)
  Seq0 <- Key("seq") * +(+'\n' * ' '[2] * Tag * Key("id") * Identifier *
            +(+'\n' * ' '[4] * Attr * B))
  Instances0 <- Key("instances") * +(+'\n' * ' '[2] * Key(Identifier) *
            +(+'\n' * ' '[4] * Attr * B))
  Types0 <- Key("types") *
            +(+'\n' * ' '[2] * Key(Identifier) * +(+'\n' * Section4))
  Enums0 <- Key("enums") * +(+'\n' * ' '[2] * Key(Identifier) *
            +(+'\n' * ' '[4] * Key(+Digit) * Identifier))

  Meta4 <- Key("meta") * +'\n' * +(' '[6] * MetaAttrs2 * +'\n')
  Doc4 <- Key("doc") * (('|' * B * *(+'\n' * ' '[6] * Any)) | Any)
  #DocRef(n) <- Key("doc-ref") * Any
  Seq4 <- Key("seq") * +(+'\n' * ' '[6] * Tag * Key("id") * Identifier *
            +(+'\n' * ' '[8] * Attr * B))
  Instances4 <- Key("instances") * +(+'\n' * ' '[6] * Key(Identifier) *
                +(+'\n' * ' '[8] * Attr * B))
  Enums4 <- Key("enums") * +(+'\n' * ' '[6] * Key(Identifier) *
            +(+'\n' * ' '[8] * Key(+Digit) * Identifier))

  # Section Attributes
  MetaAttrs2 <- Id | Title | Application | Ext2 | License | Imports2 | Encoding |
                Endian
  Attr <- Consume | Contents | Encoding | Endian | Enum | Enum | EosError | If |
          Include | Io | Process | Pos | Repeat | RepeatExpr | RepeatUntil |
          Size | SizeEos | Terminator | Type | Value

  # Attributes
  Application <- Key("application") * Any
  Consume <- Key("consume") * Bool
  Contents <- Key("contents") * Any #XXX
  Encoding <- Key("encoding") * Any
  Endian <- Key("endian") * ("le" | "be") * B
  Enum <- Key("enum") * Any #XXX
  EosError <- Key("eos-error") * Any #XXX
  Ext2 <- Key("file-extension") * (+('\n' * ' '[4] * Tag * Any) | Any)
  Id <- Key("id") * Identifier
  If <- Key("if") * Any #XXX
  Imports2 <- Key("imports") * (+('\n' * ' '[4] * Tag * Import * B) | Import)
  Include <- Key("include") * Any #XXX
  Io <- Key("io") * Any #XXX
  License <- Key("license") * Any
  Process <- Key("process") * Any #XXX
  Pos <- Key("pos") * Any #XXX
  Repeat <- Key("repeat") * Any #XXX
  RepeatExpr <- Key("repeat-expr") * Any #XXX
  RepeatUntil <- Key("repeat-until") * Any #XXX
  Size <- Key("size") * Any
  SizeEos <- Key("size-eos") * Any #XXX
  Terminator <- Key("terminator") * Any #XXX
  Title <- Key("title") * Any
  Type <- Key("type") * Identifier
  Value <- Key("value") * Any

doAssert p.matchFile("test.ksy").ok

import npeg, strutils

let p = peg "ksy":
  ksy <- Usertype
  
  Usertype <- *(MetaSec | DocSec | DocRefSec | SeqSec)
              # InstancesSec | EnumsSeq | TypesSeq):

  # Atoms
  B <- *Blank
  Any <- +(1 - '\n')
  Tag <- '-' * B
  Identifier <- {'a'..'z'} * *{'a'..'z','0'..'9','_'}
  Import <- +{'A'..'Z','a'..'z','0'..'9','_','-','/'}
  key(item) <- item * B * ':' * B
  arr4(item) <- +('\n' * ' '[4] * item * B)
  arr4tagged(item) <- +('\n' * ' '[4] * Tag * item * B)

  # Attributes
  Application <- key("application") * >Any
  # Consume <-
  # Contents <-
  Doc <- key("doc") * (('|' * B * *(+'\n' * ' '[2] * Any)) | Any)
  DocRef <- key("doc-ref") * >Any
  Encoding <- key("encoding") * >Any
  Endian <- key("endian") * >("le" | "be") * B
  # EosError <-
  # enum <-
  Ext <- key("file-extension") * >(arr4tagged(Any) | Any)
  # if <-
  Id <- key("id") * >Identifier
  # Include <-
  Imports <- key("imports") * >(arr4tagged(Import) | Import)
  License <- key("license") * >Any
  # Repeat <-
  # RepeatExpr <-
  # RepeatUntil <-
  # Process <-
  Size <- key("size") * Any
  # Size-eos <-
  Title <- key("title") * Any
  # terminator <-
  Type <- key("type") * Identifier

  # Section Attributes
  MetaAttrs <- Id | Title | Application | Ext | License | Imports | Encoding |
              Endian
  SeqAttrs <- Type | Size

  # Sections
  MetaSec <- key("meta") * '\n' * +(' '[2] * MetaAttrs * '\n'):
    stdout.write $0
  DocSec <- Doc * '\n':
    stdout.write $0
  DocRefSec <- DocRef:
    stdout.write "docref = " & $0
  SeqSec <- key("seq") *
            +('\n' * ' '[2] * Tag * key("id") * >Identifier * arr4(SeqAttrs)):
    stdout.write $0

doAssert p.matchFile("test.ksy").ok

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
  Key(item) <- item * B * ':' * B
  Arr4(item) <- +('\n' * ' '[4] * item * B)
  Arr4tagged(item) <- +('\n' * ' '[4] * Tag * item * B)

  # Attributes
  Application <- Key("application") * >Any
  # Consume <-
  # Contents <-
  Doc <- Key("doc") * (('|' * B * *(+'\n' * ' '[2] * Any)) | Any)
  DocRef <- Key("doc-ref") * >Any
  Encoding <- Key("encoding") * >Any
  Endian <- Key("endian") * >("le" | "be") * B
  # EosError <-
  # enum <-
  Ext <- Key("file-extension") * >(Arr4tagged(Any) | Any)
  # if <-
  Id <- Key("id") * >Identifier
  # Include <-
  Imports <- Key("imports") * >(Arr4tagged(Import) | Import)
  License <- Key("license") * >Any
  # Repeat <-
  # RepeatExpr <-
  # RepeatUntil <-
  # Process <-
  Size <- Key("size") * Any
  # Size-eos <-
  Title <- Key("title") * Any
  # terminator <-
  Type <- Key("type") * Identifier

  # Section Attributes
  MetaAttrs <- Id | Title | Application | Ext | License | Imports | Encoding |
              Endian
  SeqAttrs <- Type | Size

  # Sections
  MetaSec <- Key("meta") * '\n' * +(' '[2] * MetaAttrs * '\n'):
    stdout.write $0
  DocSec <- Doc * '\n':
    stdout.write $0
  DocRefSec <- DocRef:
    stdout.write "docref = " & $0
  SeqSec <- Key("seq") *
            +('\n' * ' '[2] * Tag * Key("id") * >Identifier * Arr4(SeqAttrs)):
    stdout.write $0

doAssert p.matchFile("test.ksy").ok

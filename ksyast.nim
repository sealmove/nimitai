import strformat, strutils

type
  KsyNodeKind* = enum
    knkType
    knkSection
    knkInst
    knkEnum
    knkAttr
    knkKey
    knkItem
  KsyNode* = ref object
    case kind*: KsyNodeKind
    of knkType:
      typeNode*: Type
    of knkSection:
      sectionNode*: Section
    of knkInst:
      instNode*: Inst
    of knkEnum:
      enumNode*: Enum
    of knkAttr:
      attrNode*: Attr
    of knkKey:
      keyNode*: Key
    of knkItem:
      itemNode*: string
  Type* = ref object
    sections: seq[Section]
  SectionKind* = enum
    skMeta
    skDoc
    skSeq
    skTypes
    skInsts
    skEnums
  Section* = ref object
    case kind*: SectionKind
    of skMeta:
      keys*: seq[Key]
    of skDoc:
      doc*: string
    of skSeq:
      attrs: seq[Attr]
    of skTypes:
      types*: seq[Type]
    of skInsts:
      insts*: seq[Inst]
    of skEnums:
      enums*: seq[Enum]
  Inst* = ref object
    attrs: seq[Attr]
  Attr* = ref object
    id: string
    keys: seq[Key]
  Enum* = ref object
    key: int
    value: string
  KeyKind* = enum
    kkApp
    kkConsume
    kkContents
    kkEncoding
    kkEndian
    #kkEnum
    #kkEosError
    kkExts
    kkId
    #kkIf
    kkImports
    #kkInclude
    #kkIo
    kkLicence
    #kkProcess
    #kkPos
    kkRepeat
    #kkRepeatExpr
    #kkRepeatUntil
    kkSize
    #kkSizeEos
    #kkTerminator
    kkTitle
    kkType
    #kkValue
  Key* = ref object
    case kind*: KeyKind
    of kkConsume:
      consume*: bool
    of kkContents:
      contents*: seq[byte]
    of kkEndian:
      endian*: Endian
    #of kkEnum
    #of kkEosError
    of kkExts, kkImports:
      list*: seq[string]
    of kkApp, kkEncoding, kkId, kkLicence, kkTitle, kkType:
      strval*: string
    #of kkIf
    #of kkInclude
    #of kkIo
    #of kkProcess
    #of kkPos
    of kkRepeat:
      repeat*: Repeat
    #of kkRepeatExpr
    #of kkRepeatUntil
    of kkSize:
      size*: int64
    #of kkSizeEos
    #of kkTerminator
    #of kkValue:
  Endian* = enum
    le
    be
  Repeat* = enum
    expr
    eos
    until

proc stackBytes*(stack: var seq[byte], s: string) =
  if s.startsWith("\""):
    for i in countdown(s.len - 2, 1):
      stack.add(s[i].byte)
  elif s.startsWith "0x":
    let x = parseHexInt(s)
    if x > 255:
      echo "Hex number out of byte range"
      quit QuitFailure
    stack.add(x.byte)
  else:
    let x = parseInt(s)
    if x > 255:
      echo "Hex number out of byte range"
      quit QuitFailure
    stack.add(x.byte)

# Debug proc
proc `$`*(n: KsyNode): string =
  case n.kind
  #of knkType:
  #of knkSection:
  #of knkInst:
  #of knkEnum:
  #of knkAttr:
  of knkKey:
    case n.keyNode.kind
    of kkContents:
      fmt"Contents({n.keyNode.contents})"
    of kkImports:
      fmt"Imports({n.keyNode.list})"
    else:
      "UNKNOWN" #XXX
  of knkItem:
    n.itemNode
  else:
    "UNKNOWN" #XXX

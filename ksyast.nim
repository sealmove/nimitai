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
    meta: seq[Key]
    doc: string
    attrs: seq[Attr]
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
    kkLicense
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
    of kkApp, kkEncoding, kkId, kkLicense, kkTitle, kkType:
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

# Helper procs
proc newKey*(kind: KeyKind, s: string = "",
            blist: seq[byte] = @[], slist: seq[string] = @[]): KsyNode =
  var k = Key(kind: kind)
  case kind
  of kkApp, kkEncoding, kkId, kkLicense, kkTitle, kkType:
    k.strval = s
  of kkConsume:
    k.consume = parseBool(s)
  of kkContents:
    k.contents.add blist
  of kkEndian:
    k.endian = if s == "le": le else: be
  of kkExts, kkImports:
    k.list.add slist
  of kkRepeat:
    k.repeat = if s == "expr": expr elif s == "eos": eos else: until
  of kkSize:
    discard
  result = KsyNode(kind: knkKey, keyNode: k)

proc stackBytes*(stack: var seq[byte], s: string) =
  if s.startsWith("\""):
    for i in countdown(s.len - 2, 1):
      stack.insert(s[i].byte, 0)
  elif s.startsWith "0x":
    let x = parseHexInt(s)
    if x > 255:
      echo "Hex number out of byte range"
      quit QuitFailure
    stack.insert(x.byte, 0)
  else:
    let x = parseInt(s)
    if x > 255:
      echo "Hex number out of byte range"
      quit QuitFailure
    stack.insert(x.byte, 0)

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
    of kkApp:
      &"App: \"{n.keyNode.strval}\""
    of kkConsume:
      &"Consume: {n.keyNode.consume}"
    of kkContents:
      &"Contents: {n.keyNode.contents}"
    of kkEncoding:
      &"Encoding: {n.keyNode.strval}"
    of kkEndian:
      &"Endian: {n.keyNode.endian}"
    of kkExts:
      &"Exts: {n.keyNode.list}"
    of kkId:
      &"Id: {n.keyNode.strval}"
    of kkImports:
      &"Imports: {n.keyNode.list}"
    of kkLicense:
      &"License: {n.keyNode.strval}"
    of kkRepeat:
      &"Repeat: {n.keyNode.repeat}"
    of kkSize:
      &"Size: {n.keyNode.size}"
    of kkTitle:
      &"Title: \"{n.keyNode.strval}\""
    of kkType:
      &"Type: {n.keyNode.strval}"
  of knkItem:
    n.itemNode
  else:
    "UNKNOWN" #XXX

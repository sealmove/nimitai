import strformat, strutils, tables

type
  Type* = ref object
    name*: string
    parent*: Type
    root*: Type
    meta*: Table[KeyKind, Key]
    doc*: string
    attrs*: seq[Attr]
    insts*: seq[Inst]
    types*: seq[Type]
    enums*: seq[Enum]
  SectKind* = enum
    skMeta
    skDoc
    skSeq
    skTypes
    skInsts
    skEnums
  Sect* = ref object
    case kind*: SectKind
    of skMeta:
      keys*: Table[KeyKind, Key]
    of skDoc:
      doc*: string
    of skSeq:
      attrs*: seq[Attr]
    of skTypes:
      types*: seq[Type]
    of skInsts:
      insts*: seq[Inst]
    of skEnums:
      enums*: seq[Enum]
  Inst* = ref object
    name*: string
    keys*: Table[KeyKind, Key]
  Attr* = ref object
    id*: string
    keys*: Table[KeyKind, Key]
  Enum* = ref object
    name*: string
    pairs*: seq[tuple[key: int, value: string]]
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
proc stackBytes*(stack: var seq[byte], s: string) =
  if s.startsWith("\""):
    for i in countdown(s.len - 2, 1):
      stack.insert(s[i].byte)
  elif s.startsWith "0x":
    let x = parseHexInt(s)
    if x > 255:
      echo "Hex number out of byte range"
      quit QuitFailure
    stack.insert(x.byte)
  else:
    let x = parseInt(s)
    if x > 255:
      echo "Hex number out of byte range"
      quit QuitFailure
    stack.insert(x.byte)

proc push*(stack: var seq[Key], kind: KeyKind, s: string = "",
          blist: seq[byte] = @[], slist: seq[string] = @[]) =
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
  stack.add k

# Debug procs
proc ksyError*(msg: string) =
  echo msg
  quit QuitFailure

proc `$`*(k: Key): string =
  case k.kind
  of kkApp:
    &"App: \"{k.strval}\""
  of kkConsume:
    &"Consume: {k.consume}"
  of kkContents:
    &"Contents: {k.contents}"
  of kkEncoding:
    &"Encoding: {k.strval}"
  of kkEndian:
    &"Endian: {k.endian}"
  of kkExts:
    &"Exts: {k.list}"
  of kkId:
    &"Id: {k.strval}"
  of kkImports:
    &"Imports: {k.list}"
  of kkLicense:
    &"License: {k.strval}"
  of kkRepeat:
    &"Repeat: {k.repeat}"
  of kkSize:
    &"Size: {k.size}"
  of kkTitle:
    &"Title: \"{k.strval}\""
  of kkType:
    &"Type: {k.strval}"

proc `$`*(t: Type): string =
  &"{t.name}"

proc `$`*(s: Sect): string =
  case s.kind:
  of skMeta:
    &"meta: {s.keys}"
  of skDoc:
    &"doc: {s.doc}"
  of skSeq:
    &"seq: (...)"
  of skTypes:
    &"types: {s.types}"
  of skInsts:
    &"insts: (...)"
  of skEnums:
    &"enums: (...)"

proc `$`*(i: Inst): string =
  &"{i.name}"

proc `$`*(e: Enum): string =
  &"{e.name}"

proc `$`*(a: Attr): string =
  &"{a.id}: {a.keys}"

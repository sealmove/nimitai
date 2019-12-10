import ksyast, tables, strutils, sequtils

type
  Nimitype* = ref object
    id*: string
    parent*: string
    root*: string
    title*: string
    app*: string
    imports*: seq[string]
    encoding*: string
    endian*: Endian
    license*: string
    exts*: seq[string]
    doc*: string
    docRef*: string
    fields*: seq[Field]
    enums*: seq[Enum]
  FieldKind* = enum
    fkNone
    fkInteger
    fkArray
    fkStrz
  ArrayKind* = enum
    akNone
    akByte
    akString
  Field* = ref object
    id*: string
    doc*: string
    docRef*: string
    contents*: seq[byte]
    typ*: string
    repeat*: Repeat
    repeatExpr*: Expression
    repeatUntil*: Expression
    ifExpr*: Expression
    case kind*: FieldKind
    of fkInteger:
      label*: string
    of fkArray:
      size*: int64
      sizeEos*: bool
      case arrayKind*: ArrayKind
      of akByte:
        process*: Process
      of akString:
        encoding*: string # might change to a big fat enum
      of akNone: discard
    of fkStrz:
      terminator*: byte
      consume*: bool
      includeTerminator*: bool
      eosError*: bool
    of fkNone: discard
    case isLazy*: bool
    of true:
      pos*: int64
      io: string
      value: int
    of false: discard
  Expression* = ref object
  Repeat* = enum
    rExpr
    rEos
    rUntil
  Process* = enum
    pXor
  Enum* = ref object
    name*: string
    pairs*: Table[string, int]
  Endian* = enum
    eLe
    eBe

proc hierarchy(t: Type): seq[string] =
  var t = t
  while t.name != "RootObj":
    result.insert(t.name.capitalizeAscii)
    t = t.parent

proc attrType(a: Attr, t: Type): string =
  case a.keys[kkType].strval
  of "u1", "u2", "u2le", "u2be", "u4", "u4le", "u4be", "u8", "u8le", "u8be",
     "s1", "s2", "s2le", "s2be", "s4", "s4le", "s4be", "s8", "s8le", "s8be":
    return a.keys[kkType].strval
  else:
    var t = t
    let typ = a.keys[kkType].strval
    while typ notin t.types.mapIt(it.name):
      t = t.parent
    return hierarchy(t).join & typ.capitalizeAscii

proc ensureMissing(a: Attr, kkList: varargs[KeyKind]) =
  for kk in kkList:
    if kk in a.keys:
      echo "Some keys could not be combined"
      quit QuitFailure

proc determineFieldKind(a: Attr): FieldKind =
  if kkEnum in a.keys:
    a.ensureMissing(kkSize, kkSizeEos, kkProcess, kkEncoding, kkTerminator,
                    kkConsume, kkInclude, kkEosError)
    return fkInteger
  if kkSize in a.keys or
     kkSizeEos in a.keys:
    a.ensureMissing(kkTerminator, kkConsume, kkInclude, kkEosError)
    return fkArray
  if kkTerminator in a.keys or
     kkConsume in a.keys or
     kkInclude in a.keys or
     kkEosError in a.keys:
    a.ensureMissing(kkEnum, kkProcess, kkEncoding)
    return fkStrz
  return fkNone

proc determineArrayKind(a: Attr): ArrayKind =
  if kkProcess in a.keys:
    a.ensureMissing(kkEncoding)
    return akByte
  if kkEncoding in a.keys:
    return akString
  return akNone

proc parseAttr(a: Attr, currentType: Type): Field =
  result = Field(kind: determineFieldKind(a))
  result.id = a.id
  if kkDoc in a.keys:
    result.doc = a.keys[kkDoc].strval
  if kkContents in a.keys:
    for s in a.keys[kkContents].list:
      if s.startsWith("\""):
        for i in 1 .. s.len - 2:
          result.contents.add(s[i].byte)
      elif s.startsWith "0x":
        let x = parseHexInt(s)
        if x > 255:
          echo "Hex number out of byte range"
          quit QuitFailure
        result.contents.add(x.byte)
      else:
        let x = parseInt(s)
        if x > 255:
          echo "Hex number out of byte range"
          quit QuitFailure
        result.contents.add(x.byte)
  if kkType in a.keys:
    result.typ = attrType(a, currentType)
  if kkRepeat in a.keys:
    case a.keys[kkRepeat].strval
    of "expr":
      if kkRepeatExpr notin a.keys:
        echo "Attribute has \"repeat: expr\" key but not \"repeat-expr\""
        quit QuitFailure
      result.repeat = rExpr
      result.repeatExpr = new(Expression) #XXX
    of "eos":
      result.repeat = rEos
    of "until":
      if kkRepeatUntil notin a.keys:
        echo "Attribute has \"repeat: until\" key but not \"repeat-until\""
        quit QuitFailure
      result.repeat = rUntil
      result.repeatUntil = new(Expression) #XXX
  if kkIf in a.keys:
    result.ifExpr = new(Expression) #XXX
  case result.kind
  of fkInteger:
    result.label = a.keys[kkEnum].strval
  of fkArray:
    if kkSize in a.keys:
      a.ensureMissing(kkSizeEos)
      result.size = parseInt(a.keys[kkSize].strval)
    elif kkSizeEos in a.keys:
      result.sizeEos = true
    result.arrayKind = determineArrayKind(a)
    case result.arrayKind
    of akByte:
      result.process = pXor #XXX
    of akString:
      result.encoding = "encoding" #XXX
    of akNone: discard
  of fkStrz:
    if kkTerminator in a.keys:
      result.terminator = parseInt(a.keys[kkTerminator].strval).byte
    if kkConsume in a.keys:
      result.consume = parseBool(a.keys[kkConsume].strval)
    if kkInclude in a.keys:
      result.includeTerminator = parseBool(a.keys[kkInclude].strval)
    if kkEosError in a.keys:
      result.eosError = parseBool(a.keys[kkEosError].strval)
  else: discard

proc parseType(ntlist: var seq[Nimitype], t: Type) =
  for typ in t.types:
    parseType(ntlist, typ)

  let nt = new(Nimitype)
  var h = hierarchy(t)
  nt.id = h.join
  discard h.pop
  nt.parent = h.join
  nt.root = t.root.name.capitalizeAscii
  if kkTitle in t.meta:
    nt.title = t.meta[kkTitle].strval
  if kkApp in t.meta:
    nt.app = t.meta[kkApp].strval
  if kkImports in t.meta:
    nt.imports = t.meta[kkImports].list
  if kkEncoding in t.meta:
    nt.encoding = t.meta[kkEncoding].strval
  if kkEndian in t.meta:
    let endian = t.meta[kkEndian].strval
    nt.endian = if endian == "le": eLe else: eBe
  if kkLicense in t.meta:
    nt.license = t.meta[kkLicense].strval
  if kkExts in t.meta:
    nt.exts = t.meta[kkExts].list
  nt.doc = t.doc
  for a in t.attrs:
    nt.fields.add parseAttr(a, t)
  #for i in t.insts:
  #  result.fields.add(parseInst(i))
  nt.enums = t.enums
  ntlist.add nt

proc parseKsyAst*(path: string): seq[Nimitype] =
  let ksy = parseKsy(path)
  result.parseType(ksy)

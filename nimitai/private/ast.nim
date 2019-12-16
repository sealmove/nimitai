import tables, strutils, sequtils, npeg, ksyast

# KSY creates the symbol table which KSEL uses; so they have to be defined
# in the same file because of cyclic dependency issue
type
  # Kaitai Struct YAML AST
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
    typ*: KsType
    repeat*: Repeat
    repeatExpr*: string
    repeatUntil*: string
    ifExpr*: string
    case kind*: FieldKind
    of fkInteger:
      label*: string
    of fkArray:
      size*: string
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
      value: string
    of false: discard
  Repeat* = enum
    rExpr
    rEos
    rUntil
  Process* = enum
    pXor
  Endian* = enum
    eNone
    eLe
    eBe

  # Kaitai Struct Expression Language AST
  KsTypeKind* = enum
    ktkNil
    ktkBit
    ktkInt
    ktkFloat
    ktkBool
    ktkString
    ktkEnum
    ktkUser
  KsType* = ref object
    case kind*: KsTypeKind
    of ktkBit:
      bits*: int
    of ktkInt:
      size*: int
      isSigned*: bool
    of ktkFloat:
      precision*: int
    of ktkNil, ktkBool, ktkString:
      discard
    of ktkEnum:    #XXX maybe should just hold an int here?
      en*: string  #XXX ???
      val*: string #XXX ???
    of ktkUser:
      id*: string
  ArithOp* = enum
    aoAdd
    aoSub
    aoMult
    aoDiv
    aoMod
  BitOp* = enum
    boLShift
    boRShift
    boBitOr
    boBitXor
    boBitAnd
  CmpOp* = enum
    coEq
    coNotEq
    coLt
    coLtE
    coGt
    coGtE
  RelOp* = enum
    roAnd
    roOr
  UnaryOp* = enum
    uoInvert
    uoNot
    uoMinus
  KsNodeKind* = enum
    knkIdentifier
    knkLiteral
    knkArithOp
    knkBitOp
    knkCmpOp
    knkRelOp
    knkUnaryOp
  KsNode* = ref object
    case kind: KsNodeKind
    of knkIdentifier:
      id*: string
    of knkLiteral:
      typ*: KsType
      val*: string
    of knkArithOp:
      ao*: ArithOp
    of knkBitOp:
      bo*: BitOp
    of knkCmpOp:
      co*: CmpOp
    of knkRelOp:
      ro*: RelOp
    of knkUnaryOp:
      uo*: UnaryOp

var nimitypeTable* {.compileTime.}: seq[Nimitype]

proc parseKsExpr*(expr: string): KsNode =
  let p = peg(kse, e: KsNode):
    kse      <- Lexeme *(B * BinaryOp * B * Lexeme):

    Lexeme   <- >?UnaryOp * >(Id | Literal):
    Id       <- Lower * *(Alnum | '_'):
    Literal  <- Float | Int | Bool | String:

    String   <- '\'' * *(Print - '\'') | '\"' * *(Print - '\"'):
    Bool     <- "true" | "false":
    Float    <- Int * '.' * Int * ?('e' * Int):
    Int      <- Hex | Bin | Dec:

    Hex      <- "0x" * +Xdigit:
    Bin      <- "0b" * +{'0', '1'}:
    Dec      <- +Digit:

    UnaryOp  <- >("+"|"-"|"not")
    BinaryOp <- ArithOp | BitOp | CmpOp | RelOp

    ArithOp  <- "+" | "-" | "*" | "/" | "%"
    BitOp    <- "<<" | ">>"| "&" | "|" | "^"
    CmpOp    <- "<=" | "<" | ">=" | ">" | "==" | "!="
    RelOp    <- "and" | "or"

    B        <- *Blank

#XXX
proc deriveNimType*(expr: KsNode): string =

proc hierarchy(t: Type): seq[string] =
  var t = t
  while t.id != "RootObj":
    result.insert(t.id.capitalizeAscii)
    t = t.parent

proc endian(t: Type): Endian =
  if kkEndian in t.meta:
    if t.meta[kkEndian].strval == "le": eLe else: eBe
  else:
    eNone

proc isBitType*(typ: string): tuple[isBit: bool, bits: int] =
  let p = peg(t, bits: int):
    t <- 'b' * >+Digit:
      bits = parseInt($0)
  var
    isBit: bool
    bits: int
  isBit = p.match(typ, bits).ok
  (isBit, bits)

proc fieldType(f: Attr|Inst, t: Type): KsType =
  let typ = f.keys[kkType].strval
  case typ
  of "u1":
    result = KsType(kind: ktkInt, size: 1, isSigned: false)
  of "u2", "u2le", "u2be":
    result = KsType(kind: ktkInt, size: 2, isSigned: false)
  of "u4", "u4le", "u4be":
    result = KsType(kind: ktkInt, size: 4, isSigned: false)
  of "u8", "u8le", "u8be":
    result = KsType(kind: ktkInt, size: 8, isSigned: false)
  of "s1":
    result = KsType(kind: ktkInt, size: 1, isSigned: true)
  of "s2", "s2le", "s2be":
    result = KsType(kind: ktkInt, size: 2, isSigned: true)
  of "s4", "s4le", "s4be":
    result = KsType(kind: ktkInt, size: 4, isSigned: true)
  of "s8", "s8le", "s8be":
    result = KsType(kind: ktkInt, size: 8, isSigned: true)
  of "f4":
    result = KsType(kind: ktkFloat, precision: 4)
  of "f8":
    result = KsType(kind: ktkFloat, precision: 8)
  of "str", "strz":
    result = KsType(kind: ktkString)
  else:
    var (isBit, bits) = isBitType(typ)
    if isBit:
      result = KsType(kind: ktkBit, bits: bits)
    else:
      #XXX user type
      var t = t
      while typ notin t.types.mapIt(it.id):
        t = t.parent
      result = KsType(kind: ktkUser, id: hierarchy(t).join & typ.capitalizeAscii)
      #XXX do enums

proc ensureMissing(f: Attr|Inst, kkList: varargs[KeyKind]) =
  for kk in kkList:
    if kk in f.keys:
      echo "Some keys could not be combined"
      quit QuitFailure

proc determineFieldKind(f: Attr|Inst): FieldKind =
  if kkEnum in f.keys:
    f.ensureMissing(kkSize, kkSizeEos, kkProcess, kkEncoding, kkTerminator,
                    kkConsume, kkInclude, kkEosError)
    return fkInteger
  if kkSize in f.keys or
     kkSizeEos in f.keys:
    f.ensureMissing(kkTerminator, kkConsume, kkInclude, kkEosError)
    return fkArray
  if kkTerminator in f.keys or
     kkConsume in f.keys or
     kkInclude in f.keys or
     kkEosError in f.keys:
    f.ensureMissing(kkEnum, kkProcess, kkEncoding)
    return fkStrz
  return fkNone

proc determineArrayKind(f: Attr|Inst): ArrayKind =
  if kkProcess in f.keys:
    f.ensureMissing(kkEncoding)
    return akByte
  if kkEncoding in f.keys:
    return akString
  return akNone

proc parseField(f: Attr|Inst, isLazy: bool, currentType: Type): Field =
  result = Field(kind: determineFieldKind(f))
  result.id = f.id
  if kkDoc in f.keys:
    result.doc = f.keys[kkDoc].strval
  if kkContents in f.keys:
    for s in f.keys[kkContents].list:
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
  if kkType in f.keys:
    result.typ = fieldType(f, currentType)
  else:
    result.typ = KsType(kind: ktkNil)
  if kkRepeat in f.keys:
    case f.keys[kkRepeat].strval
    of "expr":
      if kkRepeatExpr notin f.keys:
        echo "Attribute has \"repeat: expr\" key but not \"repeat-expr\""
        quit QuitFailure
      result.repeat = rExpr
      result.repeatExpr = "" #XXX
    of "eos":
      result.repeat = rEos
    of "until":
      if kkRepeatUntil notin f.keys:
        echo "Attribute has \"repeat: until\" key but not \"repeat-until\""
        quit QuitFailure
      result.repeat = rUntil
      result.repeatUntil = "" #XXX
  if kkIf in f.keys:
    result.ifExpr = "" #XXX
  case result.kind
  of fkInteger:
    result.label = f.keys[kkEnum].strval
  of fkArray:
    if kkSize in f.keys:
      f.ensureMissing(kkSizeEos)
      result.size = f.keys[kkSize].strval
    elif kkSizeEos in f.keys:
      result.sizeEos = true
    result.arrayKind = determineArrayKind(f)
    case result.arrayKind
    of akByte:
      result.process = pXor #XXX
    of akString:
      result.encoding = "encoding" #XXX
    of akNone: discard
  of fkStrz:
    if kkTerminator in f.keys:
      result.terminator = parseInt(f.keys[kkTerminator].strval).byte
    if kkConsume in f.keys:
      result.consume = parseBool(f.keys[kkConsume].strval)
    if kkInclude in f.keys:
      result.includeTerminator = parseBool(f.keys[kkInclude].strval)
    if kkEosError in f.keys:
      result.eosError = parseBool(f.keys[kkEosError].strval)
  else: discard
  if isLazy:
    result.isLazy = true
    if kkValue in f.keys:
      let value = f.keys[kkValue].strval
      result.value = value
      result.typ = parseKsExpr(value).deriveNimType
    #pos*: int64
    #io: string
    #value: int

proc parseType(t: Type) =
  for typ in t.types:
    parseType(typ)

  let nt = new(Nimitype)
  var h = hierarchy(t)
  nt.id = h.join
  discard h.pop
  nt.parent = h.join
  nt.root = t.root.id.capitalizeAscii
  if kkTitle in t.meta:
    nt.title = t.meta[kkTitle].strval
  if kkApp in t.meta:
    nt.app = t.meta[kkApp].strval
  if kkImports in t.meta:
    nt.imports = t.meta[kkImports].list
  if kkEncoding in t.meta:
    nt.encoding = t.meta[kkEncoding].strval
  if kkEndian in t.meta:
    nt.endian = endian(t)
  else:
    if t.id != "RootObj":
      nt.endian = endian(t.parent)
  if kkLicense in t.meta:
    nt.license = t.meta[kkLicense].strval
  if kkExts in t.meta:
    nt.exts = t.meta[kkExts].list
  nt.doc = t.doc
  for a in t.attrs:
    nt.fields.add parseField(a, false, t)
  for i in t.insts:
    nt.fields.add parseField(i, true, t)
  nt.enums = t.enums
  nimitypeTable.add nt

proc parseKsyAst*(path: string) =
  let ksy = parseKsy(path)
  parseType(ksy)

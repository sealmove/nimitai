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
  Field* = ref object
    id*: string
    doc*: string
    docRef*: string
    contents*: seq[byte]
    endian*: Endian
    typ*: KsType
    size*: KsNode
    repeat*: Repeat
    repeatExpr*: KsNode
    repeatUntil*: KsNode
    ifExpr*: KsNode
    case isLazy*: bool
    of true:
      pos*: KsNode
      io*: string
      value*: KsNode
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
  Radix* = enum
    rBin
    rDec
    rHex
  KsTypeKind* = enum
    ktkBit
    ktkBool
    ktkInt
    ktkFloat
    ktkArray
    ktkStr
    ktkStrz
    ktkUser
  KsType* = ref object
    case kind*: KsTypeKind
    of ktkBit:
      bits*: int
    of ktkBool:
      discard
    of ktkInt:
      size*: int
      isSigned*: bool
      label: string
    of ktkFloat:
      precision*: int
    of ktkArray:
      arrType*: KsType
      sizeEos*: bool
      process*: Process
    of ktkStr:
      lenEos*: bool
      encoding*: string
    of ktkStrz:
      terminator*: char
      consume*: bool
      includeTerminator*: bool
      eosError*: bool
    of ktkUser:
      id*: string
  Infix* = enum
    iAdd
    iSub
    iMul
    iDiv
    iMod
    iShl
    iShr
    iBwAnd
    iBwOr
    iBwXor
    iGtE
    iGt
    iLtE
    iLt
    iEq
    iNEq
    iAnd
    iOr
  Prefix* = enum
    pSub
    pInv
    pNot
  KsNodeKind* = enum
    knkIdentifier
    knkBool
    knkInt
    knkFloat
    knkStr
    knkArray
    knkInfix
    knkPrefix
  KsNode* = ref object
    case kind*: KsNodeKind
    of knkIdentifier:
      id*: string
    of knkBool:
      boolval*: bool
    of knkInt:
      intval*: BiggestUInt
    of knkFloat:
      floatval*: float
    of knkStr:
      strval*: string
    of knkArray:
      arrval*: seq[KsNode]
    of knkInfix:
      left*: KsNode
      inOp*: Infix
      right*: KsNode
    of knkPrefix:
      op*: KsNode
      preOp*: Prefix

var symbolTable* {.compileTime.}: Table[string, Field]

proc parseKsExpr*(expr: string): KsNode =
  var stackItems: int
  let p = peg(kse, stack: seq[KsNode]):
    kse      <- Expr

    B        <- *Blank
    CS       <- 0:
      stackItems = stack.len

    Expr     <- B * (PExpr | BExpr)
    BExpr    <- (UnaryOp | Lexeme) * B * *BinaryOp
    PExpr    <- '(' * B * Expr * B * ')'

    Lexeme   <- Literal | Id
    Id       <- Lower * *(Alnum | '_'):
      stack.add KsNode(kind: knkIdentifier, id: $0)
    Literal  <- QExpr | Array | Int | Float | Bool | String | Bytes

    QExpr    <- '\'' * >*(Print - '\'') * '\'':
      stack.add parseKsExpr($1)

    Array    <- '[' * CS * B * Expr * *(B * ',' * B * Expr) * ']':
      var arr = KsNode(kind: knkArray)
      for _ in 0 ..< stack.len - stackItems:
        arr.arrval.add(stack.pop)
      stack.add arr

    Int      <- Hex | Bin | Dec
    Hex      <- "0x" * +(Xdigit | '_'):
      stack.add KsNode(kind: knkInt, intval: parseHexInt($0).BiggestUInt)
                       
    Bin      <- "0b" * +{'0', '1', '_'}:
      stack.add KsNode(kind: knkInt, intval: parseBinInt($0).BiggestUInt)
    Dec      <- +(Digit | '_'):
      stack.add KsNode(kind: knkInt, intval: parseBiggestUInt($0))

    Float    <- Int * '.' * Int * ?('e' * Int):
      stack.add KsNode(kind: knkFloat, floatval: parseFloat($0))
    Bool     <- "true" | "false":
      stack.add KsNode(kind: knkBool, boolval: parseBool($0))
    String <- '\"' * >*(Print - '\"') * '\"':
      stack.add KsNode(kind: knkStr, strval: $1)
    Bytes <- +Print:
      var arr = KsNode(kind: knkArray)
      for c in $0:
        arr.arrval.add(KsNode(kind: knkInt, intval: c.BiggestUInt))
      stack.add arr


    UnaryOp  <- >("-"|"~"|"not") * B * (Lexeme | PExpr):
      var op: Prefix
      case $1
      of "-"  : op = pSub
      of "~"  : op = pInv
      of "not": op = pNot
      let l = pop(stack)
      stack.add KsNode(kind:knkPrefix, op: l, preOp: op)
    BinaryOp <- >("+" | "-"  | "*" | "/"  | "%" | "<<" | ">>" |  "&"  | "|" |
                  "^" | ">=" | ">" | "<=" | "<" | "==" | "!=" | "and" | "or") *
                B * Expr:
      var op: Infix
      case $1
      of "+"  : op = iAdd
      of "-"  : op = iSub
      of "*"  : op = iMul
      of "/"  : op = iDiv
      of "%"  : op = iMod
      of "<<" : op = iShl
      of ">>" : op = iShr
      of "&"  : op = iBwAnd
      of "|"  : op = iBwOr
      of "^"  : op = iBwXor
      of ">=" : op = iGtE
      of ">"  : op = iGt
      of "<=" : op = iLtE
      of "<"  : op = iLt
      of "==" : op = iEq
      of "!=" : op = iNEq
      of "and": op = iAnd
      of "or" : op = iOr
      let
        r = pop(stack)
        l = pop(stack)
      stack.add KsNode(kind: knkInfix, left: l, inOp: op, right: r)

  var stack: seq[KsNode]
  assert p.match(expr, stack).ok
  assert stack.len == 1
  stack[0]

#XXX needs recursion
proc deriveType*(expr: KsNode): KsType =
  case expr.kind
  of knkIdentifier:
    result = symbolTable[expr.id].typ
  of knkBool:
    result = KsType(kind: ktkBool)
  of knkInt:
    result = KsType(kind: ktkInt, isSigned: true)
  of knkFloat:
    result = KsType(kind: ktkFloat)
  of knkStr:
    result = KsType(kind: ktkStr)
  of knkArray:
    result = KsType(kind: ktkArray, arrType: expr.arrval[0].deriveType)
  of knkInfix:
    result = deriveType(expr.left)
  of knkPrefix:
    result = deriveType(expr.op)

proc hierarchy(t: Type): seq[string] =
  var t = t
  while t.id != "RootObj":
    result.insert(t.id.capitalizeAscii)
    t = t.parent

proc endian(s: string): Endian =
  if s == "le": eLe else: eBe

proc isBitType*(typ: string): tuple[isBit: bool, bits: int] =
  let p = peg(t, bits: int):
    t <- 'b' * >+Digit:
      bits = parseInt($1)
  var
    isBit: bool
    bits: int
  isBit = p.match(typ, bits).ok
  (isBit, bits)

proc parseField(f: Attr|Inst, isLazy: bool, currentType: Type): Field =
  result = Field(id: f.id, isLazy: isLazy)
  if kkContents in f.keys:
    let contents = f.keys[kkContents].list
    for i in contents:
      if i.startsWith('"') or i.startsWith('\''):
        for c in i[1..^2]:
          result.contents.add c.byte
      elif i.startsWith("0x"):
        result.contents.add i.parseHexInt.byte
      elif i.allIt(it in '0'..'9'):
        result.contents.add i.parseInt.byte
      else:
        for c in i:
          result.contents.add c.byte
    result.size = KsNode(kind: knkInt, intval: result.contents.len.BiggestUInt)
  if kkDoc in f.keys:
    result.doc = f.keys[kkDoc].strval
  if kkDocRef in f.keys:
    result.docRef = f.keys[kkDocRef].strval
  if kkType in f.keys:
    let typ = f.keys[kkType].strval
    case typ
    of "u1", "u2", "u2le", "u2be", "u4", "u4le", "u4be", "u8", "u8le", "u8be",
       "s1", "s2", "s2le", "s2be", "s4", "s4le", "s4be", "s8", "s8le", "s8be":
      result.typ = KsType(kind: ktkInt)
      if kkEnum in f.keys:
        result.typ.label = f.keys[kkEnum].strval
      if typ.startsWith('s'):
        result.typ.isSigned = true
      result.typ.size = parseInt(typ[1..1])
      if typ.len > 2:
        result.endian = endian(typ[2..3])
    of "f4", "f8":
      result.typ = KsType(kind: ktkFloat)
      result.typ.precision = parseInt(typ[1..1])
    of "str":
      var
        lenEos: bool
        encoding: string
      if kkSizeEos in f.keys:
        lenEos = parseBool(f.keys[kkSizeEos].strval)
      if kkEncoding in f.keys:
        encoding = f.keys[kkEncoding].strval
      result.typ = KsType(kind: ktkStr, lenEos: lenEos, encoding: encoding)
    of "strz":
      var
        terminator: char
        consume: bool
        includeTerminator: bool
        eosError: bool
      if kkTerminator in f.keys:
        terminator = f.keys[kkTerminator].charval
      if kkConsume in f.keys:
        consume = parseBool(f.keys[kkConsume].strval)
      if kkInclude in f.keys:
        includeTerminator = parseBool(f.keys[kkInclude].strval)
      if kkEosError in f.keys:
        eosError = parseBool(f.keys[kkEosError].strval)
      result.typ = KsType(kind: ktkStrz,
                          terminator: terminator,
                          consume: consume,
                          includeTerminator: includeTerminator,
                          eosError: eosError)
    else:
      var (isBit, bits) = isBitType(typ)
      if isBit:
        result.typ = KsType(kind: ktkBit, bits: bits)
      else: # user type
        result.typ = KsType(kind: ktkUser)
        var t = currentType
        while typ notin t.types.mapIt(it.id):
          t = t.parent
        result.typ.id = hierarchy(t).join & typ.capitalizeAscii
  else: # no type - byte array
    #XXX do enums
    if not isLazy:
      var
        sizeEos: bool
        process: Process
      if kkSizeEos in f.keys:
        sizeEos = parseBool(f.keys[kkSizeEos].strval)
      if kkProcess in f.keys:
        case f.keys[kkProcess].strval:
        of "xor":
          process = pXor
        else: discard
      result.typ = KsType(kind: ktkArray,
                          arrType: KsType(kind: ktkInt, size: 1, isSigned: false),
                          sizeEos: sizeEos, process: process)
  if kkSize in f.keys:
    result.size = parseKsExpr(f.keys[kkSize].strval)
  # value calculated value instances
  if kkValue in f.keys:
    let val = parseKsExpr(f.keys[kkValue].strval)
    result.value = val
    result.typ = val.deriveType
  # kkIf

  # kkRepeat
  # kkRepeatExpr
  # kkRepeatUntil

  # kkIo
  if kkPos in f.keys:
    result.pos = parseKsExpr(f.keys[kkPos].strval)

proc parseType(types: var seq[Nimitype], t: Type) =
  for typ in t.types:
    parseType(types, typ)

  var h = hierarchy(t)
  let nt = new(Nimitype)
  nt.id = h.join
  discard h.pop
  nt.parent = h.join
  nt.root = t.root.id.capitalizeAscii
  if kkEndian in t.meta:
    nt.endian = endian(t.meta[kkEndian].strval)
  else:
    var pt = t
    while t.parent.id != "RootObj":
      if kkEndian in t.parent.meta:
        nt.endian = endian(t.parent.meta[kkEndian].strval)
        break
      pt = t.parent
  if kkTitle in t.meta:
    nt.title = t.meta[kkTitle].strval
  if kkApp in t.meta:
    nt.app = t.meta[kkApp].strval
  if kkImports in t.meta:
    nt.imports = t.meta[kkImports].list
  if kkEncoding in t.meta:
    nt.encoding = t.meta[kkEncoding].strval
  if kkLicense in t.meta:
    nt.license = t.meta[kkLicense].strval
  if kkExts in t.meta:
    nt.exts = t.meta[kkExts].list
  nt.doc = t.doc
  for a in t.attrs:
    let field = parseField(a, false, t)
    nt.fields.add field
    symbolTable[field.id] = field
  for i in t.insts:
    let field = parseField(i, true, t)
    nt.fields.add field
    symbolTable[field.id] = field
  nt.enums = t.enums
  types.add nt

proc parseKsyAst*(path: string): seq[Nimitype] =
  let ksy = parseKsy(path)
  result.parseType(ksy)

import macros, json, tables, strutils, strformat
import regex

type
  TypeKey* = enum
    tkId        = "id"
    tkTypes     = "types"
    tkMeta      = "meta"
    tkDoc       = "doc"
    tkDocRef    = "doc-ref"
    tkParams    = "params"
    tkSeq       = "seq"
    tkInstances = "instances"
    tkEnums     = "enums"
  Type* = ref object
    keys*       : set[TypeKey]
    supertypes* : seq[Type]
    parent*     : Type
    id*         : string
    types*      : seq[Type]
    meta*       : Meta
    doc*        : string
    docRef*     : string
    params*     : JsonNode # XXX
    seq*        : seq[Field]
    instances*  : seq[Field]
    enums*      : Table[string, OrderedTable[int, VerboseEnum]]
  MetaKey* = enum
    mkApplication   = "application"
    mkBitEndian     = "bit-endian"
    mkEncoding      = "encoding"
    mkEndian        = "endian"
    mkFileExtension = "file-extension"
    mkId            = "id"
    mkImports       = "imports"
    mkKsDebug       = "ks-debug"
    mkKsOpaqueTypes = "ks-opaque-types"
    mkKsVersion     = "ks-version"
    mkLicense       = "license"
    mkTitle         = "title"
    mkXref          = "xref"
  Meta* = ref object
    keys*          : set[MetaKey]
    id*            : string
    title*         : string
    application*   : seq[string]
    fileExtension* : seq[string]
    xref*          : JsonNode # XXX
    license*       : string
    ksVersion*     : string
    ksDebug*       : bool
    ksOpaqueTypes* : bool
    imports*       : seq[string]
    encoding*      : string
    endian*        : Endian
    bitEndian*     : Endian
  RepeatKind* = enum
    rkNone
    rkEos   = "eos"
    rkExpr  = "expr"
    rkUntil = "until"
  FieldKey* = enum
    fkConsume     = "consume"
    fkContents    = "contents"
    fkDoc         = "doc"
    fkDocRef      = "doc-ref"
    fkEncoding    = "encoding"
    fkEnum        = "enum"
    fkEosError    = "eos-error"
    fkId          = "id"
    fkIf          = "if"
    fkInclude     = "include"
    fkIo          = "io"
    fkPadRight    = "pad-right"
    fkPos         = "pos"
    fkProcess     = "process"
    fkRepeat      = "repeat"
    fkRepeatExpr  = "repeat-expr"
    fkRepeatUntil = "repeat-until"
    fkSize        = "size"
    fkSizeEos     = "size-eos"
    fkTerminator  = "terminator"
    fkType        = "type"
    fkValue       = "value"
  FieldKind* = enum
    fkAttr, fkInst
  Field* = ref object
    case kind*: FieldKind
    of fkAttr:
      discard
    of fkInst:
      pos*  : Expr
      value*: Expr
    keys*        : set[FieldKey]
    st*          : Type
    id*          : string
    doc*         : string
    docRef*      : string
    contents*    : seq[byte]
    `type`*      : KsType
    repeat*      : RepeatKind
    repeatExpr*  : Expr
    repeatUntil* : Expr
    `if`*        : Expr
    size*        : Expr
    sizeEos*     : bool
    process*     : proc()
    `enum`*      : seq[string]
    encoding*    : string
    padRight*    : byte
    terminator*  : byte
    consume*     : bool
    `include`*   : bool
    eosError*    : bool
    io*          : Expr
  VerboseEnumKey* = enum
    vekId     = "id"
    vekDoc    = "doc"
    vekDocRef = "doc-ref"
    vekOrigId = "-orig-id"
  VerboseEnum* = ref object
    keys*   : set[VerboseEnumKey]
    id*     : string
    doc*    : string
    docRef* : string
    origId* : string
  Expr* = ref object
    node* : KsNode
    st*   : Type
  KsNodeKind* = enum
    knkBool
    knkInt
    knkFloat
    knkStr
    knkOp
    knkId
    knkEnum
    knkCast
    knkArr
    knkMeth
    knkIdx
    knkDotExpr
    knkUnary
    knkInfix
    knkTernary
  KsNode* = ref object
    case kind*: KsNodeKind
    of knkBool:
      boolval*: bool
    of knkInt:
      intval*: BiggestInt
    of knkFloat:
      floatval*: float
    of knkCast:
      kstype*: KsType
    of knkEnum:
      enumscope*: seq[string]
      enumval*: string
    of knkStr, knkOp, knkId:
      strval*: string
    else:
      sons*: seq[KsNode]
  KsTypeKind* = enum
    ktkBit   = "b"
    ktkUInt  = "u"
    ktkSInt  = "s"
    ktkFloat = "f"
    ktkStr   = "str"
    ktkArr
    ktkUser
    ktkEnum
  KsType* = ref object
    case kind*: KsTypeKind
    of ktkBit:
      bits*: int
      bitEndian*: Endian
    of ktkUInt, ktkSInt, ktkFloat:
      bytes*: int
      endian*: Endian
    of ktkStr:
      isZeroTerm*: bool
    of ktkArr:
      elemtype*: KsType
    of ktkUser:
      usertype*: Type
    of ktkEnum:
      owningtype*: Type
      enumname*: string
  Endian* = enum
    eNone
    eLe = "le"
    eBe = "be"
  KaitaiError* = object of Defect

proc walkdown(typ: Type, path: seq[string]): Type =
  var
    typ = typ
    path = path
  while path != @[]:
    block walking:
      for t in typ.types:
        if eqIdent(path[^1], t.id):
          typ = t
          discard pop(path)
          break walking
      for e in typ.enums.keys:
        if eqIdent(path[^1], e):
          discard pop(path)
          break walking
      quit(fmt"Could not walk down to type '{path[^1]}' from type '{typ.id}'")
  return typ

proc follow(typ: Type, name: string, path: seq[string]): Type =
  # First search in current's type types
  for t in typ.types:
    if eqIdent(name, t.id):
      return walkdown(t, path)
  # Then search in current's type enums
  for e in typ.enums.keys:
    if eqIdent(name, e):
      return walkdown(typ, path)
  # Then check current type itself
  if eqIdent(name, typ.id):
    return walkdown(typ, path)
  # Then go one level up and try again
  if typ.parent != nil:
    return follow(typ.parent, name, path)
  # No match
  quit(fmt"Could not follow '{name}' from type '{typ.id}'")

proc tbit*(bits: int, endian = eNone): KsType =
  doAssert bits in {1..64}
  KsType(kind: ktkBit, bits: bits, bitEndian: endian)

proc tuint*(bytes: int, endian = eNone): KsType =
  doAssert bytes in {1,2,4,8}
  KsType(kind: ktkUInt, bytes: bytes, endian: endian)

proc tsint*(bytes: int, endian = eNone): KsType =
  doAssert bytes in {1,2,4,8}
  KsType(kind: ktkSInt, bytes: bytes, endian: endian)

proc tfloat*(bytes: int, endian = eNone): KsType =
  doAssert bytes in {4,8}
  KsType(kind: ktkFloat, bytes: bytes, endian: endian)

proc tstr*(isZeroTerm = false): KsType =
  KsType(kind: ktkStr, isZeroTerm: isZeroTerm)

proc tarr*(et: KsType): KsType =
  KsType(kind: ktkArr, elemtype: et)

proc tuser*(typ: Type, name: string, path: seq[string]): KsType =
  result = KsType(kind: ktkUser)
  if typ != nil:
    result.usertype = typ.follow(name, path)

proc tenum*(typ: Type, scope: seq[string]): KsType =
  let
    typename = scope[0]
    enumname = scope[^1]
  var
    scope = scope
    path: seq[string]
  discard pop(scope)
  for i in countdown(scope.len - 1, 1):
    path.add(scope[i])
  KsType(
    kind: ktkEnum,
    owningtype: if scope == @[]: typ else: typ.follow(typename, path),
    enumname: enumname)

proc parseType*(s: string, typ: Type): KsType =
  if s.match(re"u[1248](be|le)?"):
    result = tuint(parseInt(s[1..1]))
    if s.match(re".*(be|le)"):
      result.endian = parseEnum[Endian](s[2..3])
  elif s.match(re"s[1248](be|le)?"):
    result = tsint(parseInt(s[1..1]))
    if s.match(re".*(be|le)"):
      result.endian = parseEnum[Endian](s[2..3])
  elif s.match(re"f[48](be|le)?"):
    result = tfloat(parseInt(s[1..1]))
    if s.match(re".*(be|le)"):
      result.endian = parseEnum[Endian](s[2..3])
  elif s.match(re"b[1-9][0-9]*(be|le)?"):
    if s.match(re".*(be|le)"):
      result = tbit(parseInt(s[1..^3]), parseEnum[Endian](s[^2..^1]))
    else:
      result = tbit(parseInt(s[1..^1]))
  elif s.match(re"strz?"):
    result = tstr(if s.endsWith('z'): true else: false)
  else:
    var
      scope = split(s, "::")
      path: seq[string]
    let name = scope[0]
    for i in countdown(scope.len - 1, 1):
      path.add(scope[i])
    result = tuser(typ, name, path)

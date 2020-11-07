import macros, json, tables, strformat

const
  rootTypeName* = "KaitaiStruct"
  streamTypeName* = "KaitaiStream"

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
      pos*  : KsNode
      value*: KsNode
    keys*        : set[FieldKey]
    st*          : Type
    id*          : string
    doc*         : string
    docRef*      : string
    contents*    : seq[byte]
    `type`*      : KsType
    repeat*      : RepeatKind
    repeatExpr*  : KsNode
    repeatUntil* : KsNode
    `if`*        : KsNode
    size*        : KsNode
    sizeEos*     : bool
    process*     : proc()
    `enum`*      : seq[string]
    encoding*    : string
    padRight*    : byte
    terminator*  : byte
    consume*     : bool
    `include`*   : bool
    eosError*    : bool
    io*          : KsNode
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
    cx*: Type
  KsTypeKind* = enum
    ktkBit   = "b"
    ktkUInt  = "u"
    ktkSInt  = "s"
    ktkFloat = "f"
    ktkStr   = "str"
    ktkArr
    ktkUser
    ktkEnum
    ktkStream
    ktkSwitch
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
    of ktkSwitch:
      on*: KsNode
      cases*: seq[tuple[v: KsNode, t: KsType]]
    of ktkStream:
      discard
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

proc tstream*(): KsType =
  KsType(kind: ktkStream)

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

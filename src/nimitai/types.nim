import json, tables
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
      scope*: seq[string]
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

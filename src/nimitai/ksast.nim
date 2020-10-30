import macros, json, strutils, tables, sequtils, strformat
import regex
import exprlang

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
  Meta = ref object
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
    st           : Type
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

proc tbit(bits: int, endian = eNone): KsType =
  doAssert bits in {1..64}
  KsType(kind: ktkBit, bits: bits, bitEndian: endian)

proc tuint(bytes: int, endian = eNone): KsType =
  doAssert bytes in {1,2,4,8}
  KsType(kind: ktkUInt, bytes: bytes, endian: endian)

proc tsint(bytes: int, endian = eNone): KsType =
  doAssert bytes in {1,2,4,8}
  KsType(kind: ktkSInt, bytes: bytes, endian: endian)

proc tfloat(bytes: int, endian = eNone): KsType =
  doAssert bytes in {4,8}
  KsType(kind: ktkFloat, bytes: bytes, endian: endian)

proc tstr(isZeroTerm = false): KsType =
  KsType(kind: ktkStr, isZeroTerm: isZeroTerm)

proc tarr(et: KsType): KsType =
  KsType(kind: ktkArr, elemtype: et)

proc tuser(typ: Type, name: string, path: seq[string]): KsType =
  KsType(kind: ktkUser, usertype: typ.follow(name, path))

proc tenum(typ: Type, scope: seq[string]): KsType =
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

proc parseType(s: string, typ: Type): KsType =
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

proc buildNimTypeId*(typ: Type): string =
  var
    it = typ
    stack: seq[string]

  while it != nil:
    stack.add(it.id)
    it = it.parent

  while stack != @[]:
    result &= pop(stack).capitalizeAscii

proc access(typ: Type, symbol: string): KsType =
  for a in typ.seq:
    if eqIdent(symbol, a.id):
      return a.`type`
  for i in typ.instances:
    if eqIdent(symbol, i.id):
      return i.`type`
  quit(fmt"Could not access '{symbol}' from '{typ.id}'")

proc matchAndBuildEnum*(scope: seq[string], typ: Type): string =
  if scope.len > 1:
    for t in typ.types:
      if t.id == scope[0]:
        for e in t.enums.keys:
          if scope[^1] == e:
            return buildNimTypeId(t) & scope[1..^1].join
  else:
    for e in typ.enums.keys:
      if scope[^1] == e:
        return buildNimTypeId(typ) & scope.join
  if typ.parent != nil:
    return matchAndBuildEnum(scope, typ.parent)
  let e = scope.join("::")
  quit(fmt"Enum {e} not found")

proc ksToNimType*(ksType: KsType): NimNode =
  case ksType.kind
  of ktkBit:
    if ksType.bits == 1:
      result = ident"bool"
    else:
      result = ident"uint64"
  of ktkUInt:
    result = ident("uint" & $(8 * ksType.bytes))
  of ktkSInt:
    result = ident("int" & $(8 * ksType.bytes))
  of ktkFloat:
    result = ident("float" & $(8 * ksType.bytes))
  of ktkStr:
    result = ident"string"
  of ktkArr:
    result = nnkBracketExpr.newTree(ident"seq", ksToNimType(ksType.elemtype))
  of ktkUser:
    result = ident(buildNimTypeId(ksType.usertype))
  of ktkEnum:
    result = ident(buildNimTypeId(ksType.owningtype) & ksType.enumname)

proc removeLeadingUnderscores(s: var string) =
  while s[0] == '_':
    s.delete(0, 0)

proc parseBinOctDecHex(s: string): int =
  if   s.startsWith("0b"): parseBinInt(s)
  elif s.startsWith("0o"): parseOctInt(s)
  elif s.startsWith("0x"): parseHexInt(s)
  else: parseInt(s)

proc jsonToByte(json: JsonNode): byte =
  case json.kind
  of JString:
    result = json.getStr.parseBinOctDecHex.byte
  of JInt:
    let x = json.getInt
    doAssert x >= 0 and x <= 255
    result = x.byte
  else:
    result = 0

proc jsonToExpr(json: JsonNode, typ: Type): Expr =
  result = Expr(st: typ)
  case json.kind
  of JString:
    result.node = json.getStr.toKs
  of JInt:
    result.node = KsNode(kind: knkInt, intval: json.getInt)
  of JFloat:
    result.node = KsNode(kind: knkFloat, floatval: json.getFloat)
  of JBool:
    result.node = KsNode(kind: knkBool, boolval: json.getBool)
  else: discard # Should not occur

proc isByteArray(node: KsNode): bool =
  doAssert node.kind == knkArr
  for s in node.sons:
    if s.kind != knkInt or s.intval < 0x00 or s.intval > 0xff:
      return false
  return true

proc toNim*(expression: Expr): NimNode =
  let (e, st) = (expression.node, expression.st)
  case e.kind
  of knkBool:
    result = newLit(e.boolval)
  of knkInt:
    result = newIntLitNode(e.intval)
  of knkFloat:
    result = newFloatLitNode(e.floatval)
  of knkStr:
    result = newStrLitNode(e.strval)
  of knkOp:
    case e.strval
    of "%" : result = ident"mod"
    of "<<": result = ident"shl"
    of ">>": result = ident"shr"
    of "&" : result = ident"and"
    of "|" : result = ident"or"
    of "^" : result = ident"xor"
    else   : result = ident(e.strval)
  of knkId: # XXX
    result = newDotExpr(ident"this", ident(e.strval))
  of knkEnum: # XXX implement relative matching
    if st != nil:
      result = newDotExpr(
        ident(matchAndBuildEnum(e.enumscope, st)),
        ident(e.enumval))
    else: # needed for generating tests
      result = newDotExpr(
        ident(join(e.enumscope).capitalizeAscii),
        ident(e.enumval))
  of knkCast:
    discard # XXX
  of knkArr:
    let x = newTree(nnkBracket)
    for s in e.sons:
      x.add Expr(node: s, st: st).toNim
    if e.sons != @[] and e.isByteArray:
      x[0] = newLit(e.sons[0].intval.byte)
    result = prefix(x, "@")
  of knkMeth:
    result = newTree(nnkCall)
    result.add(ident(e.sons[0].strval))
    for i in 1 ..< e.sons.len:
      result.add(Expr(node: e.sons[i], st: st).toNim)
  of knkIdx:
    result = nnkBracketExpr.newTree(
      Expr(node: e.sons[0], st: st).toNim,
      Expr(node: e.sons[1], st: st).toNim)
  of knkDotExpr:
    var (l, r) = (e.sons[0], e.sons[1])
    case r.kind
    # ! special case because of Nim AST peculiarity
    of knkMeth:
      r.sons.insert(l, 1)
      result = Expr(node: r, st: st).toNim
    of knkId:
      result = newDotExpr(
        Expr(node: l, st: st).toNim,
        ident(r.strval))
    else: discard # should not occur
  of knkUnary:
    result = nnkPrefix.newTree(
      Expr(node: e.sons[0], st: st).toNim,
      Expr(node: e.sons[1], st: st).toNim)
  of knkInfix:
    result = nnkInfix.newTree(
      Expr(node: e.sons[1], st: st).toNim,
      Expr(node: e.sons[0], st: st).toNim,
      Expr(node: e.sons[2], st: st).toNim)
  of knkTernary:
    result = nnkIfStmt.newTree(
      nnkElifBranch.newTree(
        Expr(node: e.sons[0], st: st).toNim,
        Expr(node: e.sons[1], st: st).toNim),
      nnkElse.newTree(Expr(node: e.sons[2], st: st).toNim))

proc inferType(expression: Expr): KsType =
  let (node, kind, st) = (expression.node, expression.node.kind, expression.st)
  case kind
  of knkBool:
    result = tbit(1)
  # XXX what if it doesn't fit into an int? (use uint)
  of knkInt:
    case node.intval
    of -0x80 .. -0x01:
      result = tsint(1)
    of 0x00 .. 0xff:
      result = tuint(1)
    of -0x8000 .. -0x81, 0x100 .. 0x7fff:
      result = tsint(2)
    of -0x8000_0000 .. -0x8001, 0x8000 .. 0x7fff_ffff:
      result = tsint(4)
    of low(int) .. -0x8000_0001, 0x8000_0000 .. high(int):
      result = tsint(8)
  of knkFloat:
    result = tfloat(8)
  of knkStr:
    result = tstr()
  of knkOp:
    discard
  of knkId:
    result = st.access(node.strval)
  of knkEnum:
    result = tenum(st, node.enumscope)
  of knkArr:
    let types = node.sons.mapIt(inferType(Expr(node: it, st: st)))
    for i in 1 ..< types.len:
      doAssert types[0].kind == types[i].kind
    result = tarr(types[0])
  of knkMeth: # XXX
    doAssert node.sons[0].kind == knkId
    if eqIdent(node.sons[0].strval, "to_s"):
      result = tstr()
    elif eqIdent(node.sons[0].strval, "to_i"):
      result = tsint(8)
    elif eqIdent(node.sons[0].strval, "length"):
      result = tsint(8)
    elif eqIdent(node.sons[0].strval, "substring"):
      result = tstr()
    elif eqIdent(node.sons[0].strval, "size"):
      result = tsint(8)
    elif eqIdent(node.sons[0].strval, "first"):
      result = tuint(1) # XXX
    elif eqIdent(node.sons[0].strval, "last"):
      result = tuint(1) # XXX
    else:
      quit(fmt"Method {node.sons[0].strval} not found")
  of knkIdx: discard # XXX
  of knkCast: result = tstr()#discard # XXX
  of knkDotExpr:
    case node.sons[1].kind
    of knkId:
      let kt = infertype(Expr(node: node.sons[0], st: st))
      result = access(kt.usertype, node.sons[1].strval)
    of knkMeth:
      result = infertype(Expr(node: node.sons[1], st: st))
    else: discard # XXX
  of knkUnary:
    result = inferType(Expr(node: node.sons[1], st: st))
  of knkInfix:
    let (l, r) = (inferType(Expr(node: node.sons[0], st: st)),
                  inferType(Expr(node: node.sons[2], st: st)))
    case node.sons[1].strval
    of ">=", ">", "<=", "<", "==", "!=":
      result = tbit(1)
    else:
      result = l
    # XXX type algebra
    #case l.kind
    #of ktkBit
    #of ktkUInt
    #of ktkSInt
    #of ktkFloat
    #of ktkStr
    #of ktkArr
    #of ktkUser
  of knkTernary:
    let (c, t, f) = (inferType(Expr(node: node.sons[0], st: st)),
                     inferType(Expr(node: node.sons[1], st: st)),
                     inferType(Expr(node: node.sons[2], st: st)))
    doAssert (c.kind == ktkBit and c.bits == 1)
    doAssert t.kind == f.kind
    result = t

proc meta(json: JsonNode, defaults: Meta): Meta =
  result = defaults

  # keys
  for key in json.keys:
    result.keys.incl(parseEnum[MetaKey](key))

  # id
  if mkId in result.keys:
    result.id = json["id"].getStr

  # title
  if mkTitle in result.keys:
    result.title = json["title"].getStr

  # application
  if mkApplication in result.keys:
    let jnode = json["application"]
    case jnode.kind
    of JArray:
      for s in jnode.items:
        result.application.add(s.getStr)
    of JString:
      result.application.add(jnode.getStr)
    else: discard # should not occur

  # file-extension
  if mkFileExtension in result.keys:
    let jnode = json["file-extension"]
    case jnode.kind
    of JArray:
      for s in jnode.items:
        result.fileExtension.add(s.getStr)
    of JString:
      result.fileExtension.add(jnode.getStr)
    else: discard # should not occur

  # xref
  if mkXref in result.keys:
    result.xref = json["xref"] # XXX

  # license
  if mkLicense in result.keys:
    result.license = json["license"].getStr

  # ks-version
  if mkKsVersion in result.keys:
    result.ksVersion = json["ks-version"].getStr

  # ks-debug
  if mkKsDebug in result.keys:
    result.ksDebug = json["ks-debug"].getBool

  # ks-opaque-types
  if mkKsOpaqueTypes in result.keys:
    result.ksOpaqueTypes = json["ks-debug"].getBool

  # imports
  if mkImports in result.keys:
    let jnode = json["imports"]
    case jnode.kind
    of JArray:
      for s in jnode.items:
        result.imports.add(s.getStr)
    of JString:
      result.imports.add(jnode.getStr)
    else: discard # should not occur

  # encoding
  if mkEncoding in result.keys:
    result.encoding = json["encoding"].getStr

  # endian
  if mkEndian in result.keys:
    result.endian = parseEnum[Endian](json["endian"].getStr)

  # bit-endian
  if mkBitEndian in result.keys:
    result.bitEndian = parseEnum[Endian](json["bit-endian"].getStr)

proc field(kind: FieldKind, id: string, st: Type, json: JsonNode): Field =
  result = Field(kind: kind, id: id, st: st)

  # keys
  for key in json.keys:
    result.keys.incl(parseEnum[FieldKey](key))

  # doc
  if fkDoc in result.keys:
    result.doc = json["doc"].getStr

  # doc-ref
  if fkDocRef in result.keys:
    result.docRef = json["doc-ref"].getStr

  if fkContents in result.keys:
    let x = json["contents"]
    case x.kind
    of JString:
      let parsed = x.getStr.toKs
      case parsed.kind
      of knkStr:
        for c in parsed.strval:
          result.contents.add(c.byte)
      of knkArr:
        for son in parsed.sons:
          doAssert son.kind == knkInt
          result.contents.add(son.intval.byte)
      else: discard # should not occur
    of JArray:
      for e in x:
        result.contents.add(e.jsonToByte)
    else: discard # should not occur

  # type
  if fkType in result.keys:
    result.`type` = parseType(json["type"].getStr, result.st)
    if result.`type`.kind == ktkUser:
      result.`type`.usertype.supertypes.add(st)
  else:
    result.`type` = tarr(tuint(1))

  # repeat
  if fkRepeat in result.keys:
    result.repeat = parseEnum[RepeatKind](json["repeat"].getStr)

  # XXX repeat-expr

  # XXX repeat-until

  # if
  if fkIf in result.keys:
    result.`if` = jsonToExpr(json["if"], result.st)

  # size
  if fkSize in result.keys:
    result.size = jsonToExpr(json["size"], result.st)

  # size-eos
  if fkSizeEos in result.keys:
    result.sizeEos = json["size-eos"].getBool

  # XXX process

  # enum
  if fkEnum in result.keys:
    result.`enum` = split(json["enum"].getStr, "::")

  # encoding
  if fkEncoding in result.keys:
    result.encoding = json["encoding"].getStr

  # pad-right
  if fkPadRight in result.keys:
    result.padRight = jsonToByte(json["pad-right"])

  # terminator
  if fkTerminator in result.keys:
    result.terminator = jsonToByte(json["terminator"])

  # consume
  if fkConsume in result.keys:
    result.consume = json["consume"].getBool
  else:
    result.consume = true

  # include
  if fkInclude in result.keys:
    result.`include` = json["include"].getBool

  # eos-error
  if fkEosError in result.keys:
    result.eosError = json["eos-error"].getBool
  else:
    result.eosError = true

  # io XXX
  if fkSize in result.keys:
    result.io = Expr(node: KsNode(kind: knkId, strval: result.id & "io"), st: st)
  else:
    if fkIo in result.keys:
      result.io = jsonToExpr(json["io"], st)
    else:
      result.io = Expr(node: KsNode(kind: knkId, strval: "io"), st: st)

  # pos
  if fkPos in result.keys:
    result.pos = jsonToExpr(json["pos"], st)

  # value
  if fkValue in result.keys:
    result.value = jsonToExpr(json["value"], st)
    result.`type` = inferType(result.value)

proc verboseEnum(json: JsonNode): VerboseEnum =
  result = VerboseEnum()
  for key in json.keys:
    result.keys.incl(parseEnum[VerboseEnumKey](key))

  # id
  if vekId in result.keys:
    result.id = json["id"].getStr

  # doc
  if vekDoc in result.keys:
    result.doc = json["doc"].getStr

  # doc-ref
  if vekDocRef in result.keys:
    result.docRef = json["doc-ref"].getStr

  # -orig-id
  if vekDocRef in result.keys:
    result.origId = json["-orig-id"].getStr

proc fillType(typ: Type, json: JsonNode) =
  # keys
  for key in json.keys:
    typ.keys.incl(parseEnum[TypeKey](key))

  # meta
  if typ.parent == nil:
    let defaults = Meta(bitEndian: eBe)
    if tkMeta in typ.keys:
      typ.meta = meta(json["meta"], defaults)
    else:
      typ.meta = defaults
  else:
    if tkMeta in typ.keys:
      typ.meta = meta(json["meta"], Meta(bitEndian: typ.parent.meta.bitEndian))
    else:
      typ.meta = typ.parent.meta

  # enums
  if tkEnums in typ.keys:
    typ.enums = initTable[string, OrderedTable[int, VerboseEnum]]()
    for name, consts in json["enums"]:
      typ.enums[name] = initOrderedTable[int, VerboseEnum]()
      for n, ve in consts:
        case ve.kind
        of JString:
          typ.enums[name][parseBinOctDecHex(n)] = VerboseEnum(keys: {vekId}, id: ve.getStr)
        of JObject:
          typ.enums[name][parseBinOctDecHex(n)] = verboseEnum(ve)
        else: discard # should not occur

  # types
  if tkTypes in typ.keys:
    # Need to construct tree with ids and fill in the rest of the info in a
    # separate step because attributes can reference the ids from the 'type' key
    for key in json["types"].keys:
      let node = Type(id: key, parent: typ)
      typ.types.add(node)
    # This is only possible because Nim's JSON implementation uses OrderedTable
    var i: int
    for _, v in json["types"].pairs:
      fillType(typ.types[i], v)
      inc i

  # seq
  if tkSeq in typ.keys:
    for a in json["seq"].items:
      typ.seq.add(field(fkAttr, a["id"].getStr, typ, a))

  # instances
  if tkInstances in typ.keys:
    for k, v in json["instances"].pairs:
      typ.instances.add(field(fkInst, k, typ, v))

  # doc
  if tkDoc in typ.keys:
    typ.doc = json["doc"].getStr

  # doc-ref
  if tkDocRef in typ.keys:
    typ.docRef = json["doc-ref"].getStr

  # params
  typ.params = json.getOrDefault("params") # XXX

proc toType*(json: JsonNode): Type =
  if not json.hasKey("meta"):
    raise newException(KaitaiError, "Top level type has no 'meta' section")

  if not json["meta"].hasKey("id"):
    raise newException(KaitaiError, "No id in 'meta' section for top level type")

  result = Type(id: json["meta"]["id"].getStr.capitalizeAscii)
  fillType(result, json)

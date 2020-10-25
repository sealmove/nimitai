import macros, json, strutils, strformat, tables
import exprlang

type
  TypeKey* {.pure.} = enum
    id, types, meta, doc, `doc-ref`, params, seq, instances, enums
  Type* = ref object
    keys*: set[TypeKey]
    isImpureSubStruct*: bool
    parent*: Type
    id*: string
    types*: seq[Type]
    meta*: Meta
    doc*: string
    `doc-ref`*: string
    params*: JsonNode # XXX
    seq*: seq[Field]
    instances*: seq[Field]
    enums*: Table[string, OrderedTable[string, int]]
  EndianKind* {.pure.} = enum
    le, be
  MetaKey* {.pure.} = enum
    id, title, application, `file-extension`, xref, license, `ks-version`,
    `ks-debug`, `ks-opaque-types`, imports, encoding, endian, `bit-endian`
  Meta = ref object
    keys*: set[MetaKey]
    id*: string
    title*: string
    application*: seq[string]
    `file-extension`*: seq[string]
    xref*: JsonNode # XXX
    license*: string
    `ks-version`*: string
    `ks-debug`*: bool
    `ks-opaque-types`*: bool
    imports*: seq[string]
    encoding*: string
    endian*: EndianKind
    `bit-endian`*: EndianKind
  RepeatKind* {.pure.} = enum
    none, eos, expr, until
  FieldKey* {.pure.} = enum
    id, doc, `doc-ref`, contents, `type`, repeat, `repeat-expr`, `repeat-until`,
    `if`, size, `size-eos`, process, `enum`, encoding, terminator, consume,
    `include`, `eos-error`, pos, io, value, `pad-right`
  FieldKind* = enum
    fkAttr, fkInst
  Field* = ref object
    case kind*: FieldKind
    of fkAttr:
      discard
    of fkInst:
      pos*: NimNode
      value*: NimNode
    keys*: set[FieldKey]
    parentType: Type
    id*: string
    doc*: string
    `doc-ref`*: string
    contents*: seq[byte]
    `type`*: tuple[parsed: NimNode, raw: string]
    repeat*: RepeatKind
    `repeat-expr`*: NimNode
    `repeat-until`*: NimNode
    `if`*: NimNode
    size*: NimNode
    `size-eos`*: bool
    process*: proc()
    `enum`*: string
    encoding*: string
    `pad-right`*: byte
    terminator*: byte
    consume*: bool
    `include`*: bool
    `eos-error`*: bool
    io*: NimNode
  ScopedEnum = object
    scope: seq[string]
    `enum`: string
  KaitaiError* = object of Defect

proc parseBinOctDecHex(s: string): int =
  if   s.startsWith("0b"): parseBinInt(s)
  elif s.startsWith("0o"): parseOctInt(s)
  elif s.startsWith("0x"): parseHexInt(s)
  else: parseInt(s)

proc hierarchy*(typ: Type): string =
  var
    it = typ
    stack: seq[string]

  while it != nil:
    stack.add(it.id)
    it = it.parent

  while stack != @[]:
    result &= pop(stack)

proc toScopedEnum(se: string): ScopedEnum =
  var
    x = split(se, "::")
    scope: seq[string]

  for i in 1 ..< x.len:
    scope.add x[i]

  result = ScopedEnum(scope: scope, `enum`: x[^1])

proc typeLookup(ksType: string; typ: Type; shouldMark, mark: bool): string =
  for st in typ.types:
    if ksType == st.id:
      if shouldMark and mark: st.isImpureSubstruct = true
      return hierarchy(st)
  if ksType == typ.id:
    return hierarchy(typ)
  if typ.parent == nil:
    raise newException(KaitaiError, fmt"Type '{ksType}' not found")
  typeLookup(ksType, typ.parent, shouldMark, true)

proc enumLookup(ksEnum: string, typ: Type): string =
  for key in typ.enums.keys:
    if ksEnum == key:
      return hierarchy(typ) & ksEnum
  if typ.parent == nil:
    raise newException(KaitaiError, fmt"Enum '{ksEnum}' not found")
  enumLookup(ksEnum, typ.parent)

proc nativeType(ksType: string, typ: Type): NimNode =
  if ksType.isPrim: ident(ksType.toPrim)
  else: ident(typeLookup(ksType.capitalizeAscii, typ, true, false))

proc ksAsJsonToNim(json: JsonNode, context: string): NimNode =
  case json.kind
  of JString:
    result = ksAsStrToNim(json.getStr, context)
  of JInt:
    result = newLit(json.getInt)
  of JBool:
    result = newLit(json.getBool)
  of JNull:
    result = newNilLit()
  else: discard # Should not occur

proc jsonToByte(json: JsonNode): byte =
  case json.kind
  of JString:
    result = parseBinOctDecHex(json.getStr).byte
  of JInt:
    result = json.getInt.byte
  else:
    result = 0

# XXX very sloppy implementation
proc inferType(expr: NimNode, context: Type): NimNode =
  case expr.kind
  of nnkCharLit:
    result = ident"char"
  of nnkIntLit:
    result = ident"int"
  of nnkUInt8Lit:
    result = ident"byte"
  of nnkFloatLit:
    result = ident"float"
  of nnkStrLit:
    result = ident"string"
  of nnkSym:
    result = ident"bool"
  of nnkIdent:
    if eqIdent(expr, "to_s"):
      return ident"string"
    elif eqIdent(expr, "to_i"):
      return ident"int"
    elif eqIdent(expr, "length"):
      return ident"int"
    elif eqIdent(expr, "substring"):
      return ident"string"
    elif eqIdent(expr, "size"):
      return ident"int"
    elif eqIdent(expr, "first"):
      return ident"byte"
    elif eqIdent(expr, "last"):
      return ident"byte"
    # lookup type
    else:
      for a in context.seq:
        if eqIdent(expr, a.id):
          return a.`type`.parsed
      for i in context.instances:
        if eqIdent(expr, i.id):
          return i.`type`.parsed
    quit(fmt"Identifier {repr(expr)} not found")
  of nnkBracket:
    result = nnkBracketExpr.newTree(ident"seq", inferType(expr[0], context))
  of nnkBracketExpr, nnkIfStmt, nnkElse:
    result = inferType(expr[0], context)
  of nnkPrefix, nnkInfix, nnkDotExpr, nnkElifBranch:
    result = inferType(expr[1], context)
  else:
    quit(fmt"Unexpected NimNodeKind '{expr.kind}' during type inference")

proc determineImpureSubStructs(typ: Type) = discard

proc meta(json: JsonNode, defaults: Meta): Meta =
  result = defaults

  # keys
  for key in json.keys:
    result.keys.incl(parseEnum[MetaKey](key))

  # id
  if MetaKey.id in result.keys:
    result.id = json["id"].getStr

  # title
  if MetaKey.title in result.keys:
    result.title = json["title"].getStr

  # application
  if MetaKey.application in result.keys:
    let jnode = json["application"]
    case jnode.kind
    of JArray:
      for s in jnode.items:
        result.application.add(s.getStr)
    of JString:
      result.application.add(jnode.getStr)
    else: discard # should not occur

  # file-extension
  if MetaKey.`file-extension` in result.keys:
    let jnode = json["file-extension"]
    case jnode.kind
    of JArray:
      for s in jnode.items:
        result.`file-extension`.add(s.getStr)
    of JString:
      result.`file-extension`.add(jnode.getStr)
    else: discard # should not occur

  # xref
  if MetaKey.xref in result.keys:
    result.xref = json["xref"] # XXX

  # license
  if MetaKey.license in result.keys:
    result.license = json["license"].getStr

  # ks-version
  if MetaKey.`ks-version` in result.keys:
    result.`ks-version` = json["ks-version"].getStr

  # ks-debug
  if MetaKey.`ks-debug` in result.keys:
    result.`ks-debug` = json["ks-debug"].getBool

  # ks-opaque-types
  if MetaKey.`ks-opaque-types` in result.keys:
    result.`ks-opaque-types` = json["ks-debug"].getBool

  # imports
  if MetaKey.imports in result.keys:
    let jnode = json["imports"]
    case jnode.kind
    of JArray:
      for s in jnode.items:
        result.imports.add(s.getStr)
    of JString:
      result.imports.add(jnode.getStr)
    else: discard # should not occur

  # encoding
  if MetaKey.encoding in result.keys:
    result.encoding = json["encoding"].getStr

  # endian
  if MetaKey.endian in result.keys:
    result.endian = parseEnum[EndianKind](json["endian"].getStr)

  if MetaKey.`bit-endian` in result.keys:
    result.`bit-endian` = parseEnum[EndianKind](json["bit-endian"].getStr)

proc field(kind: FieldKind, id: string, parentType: Type, json: JsonNode): Field =
  result = Field(kind: kind, id: id, parentType: parentType)

  var context: string
  case kind
  of fkAttr: context = "result"
  of fkInst: context = "this"

  # keys
  for key in json.keys:
    result.keys.incl(parseEnum[FieldKey](key))

  # doc
  if FieldKey.doc in result.keys:
    result.doc = json["doc"].getStr

  # doc-ref
  if FieldKey.`doc-ref` in result.keys:
    result.`doc-ref` = json["doc-ref"].getStr

  # XXX contents

  # type XXX code feels confusing, there should be a better design
  var
    ts: string
    t = nnkBracketExpr.newTree(ident"seq", ident"byte")
  if FieldKey.`type` in result.keys:
    ts = json["type"].getStr
    t = nativeType(ts, result.parentType)
  if FieldKey.repeat in result.keys:
    t = nnkBracketExpr.newTree(ident"seq", t)
  if FieldKey.value in result.keys:
    let v = ksAsJsonToNim(json["value"], context) # XXX this is evaluated twice
    t = inferType(v, result.parentType)
  result.`type` = (t, ts)

  # repeat
  if FieldKey.repeat in result.keys:
    result.repeat = parseEnum[RepeatKind](json["repeat"].getStr)

  # XXX repeat-expr

  # XXX repeat-until

  # XXX if

  # size
  if FieldKey.size in result.keys:
    result.size = ksAsJsonToNim(json["size"], context)

  # size-eos
  if FieldKey.`size-eos` in result.keys:
    result.`size-eos` = json["size-eos"].getBool

  # XXX process

  # enum
  if FieldKey.`enum` in result.keys:
    let scoped = toScopedEnum(json["enum"].getStr)
    if scoped.scope == @[]:
      result.`enum` = enumLookup(scoped.`enum`, result.parentType)
    else:
      var `enum` = typeLookup(scoped.scope[0].capitalizeAscii,
                              result.parentType, false, false)
      for i in 1 ..< scoped.scope.len:
        `enum` &= scoped.scope[i]
      `enum` &= scoped.`enum`
      result.`enum` = `enum`

  # encoding
  if FieldKey.encoding in result.keys:
    result.encoding = json["encoding"].getStr

  # pad-right
  if FieldKey.`pad-right` in result.keys:
    result.`pad-right` = jsonToByte(json["pad-right"])

  # terminator
  if FieldKey.terminator in result.keys:
    result.terminator = jsonToByte(json["terminator"])

  # consume
  if FieldKey.consume in result.keys:
    result.consume = json["consume"].getBool
  else:
    result.consume = true

  # include
  if FieldKey.`include` in result.keys:
    result.`include` = json["include"].getBool

  # eos-error
  if FieldKey.`eos-error` in result.keys:
    result.`eos-error` = json["eos-error"].getBool
  else:
    result.`eos-error` = true

  # io
  if FieldKey.size in result.keys:
    result.io = ident(result.id & "Io")
  else:
    if FieldKey.io in result.keys:
      result.io = ksAsJsonToNim(json["io"], context)
    else:
      result.io = newDotExpr(ident(context), ident"io")

  # pos
  if FieldKey.pos in result.keys:
    result.pos = ksAsJsonToNim(json["pos"], context)

  # value
  if FieldKey.value in result.keys:
    result.value = ksAsJsonToNim(json["value"], context)

proc toKsTypeRec(typ: Type, json: JsonNode) =
  # keys
  for key in json.keys:
    typ.keys.incl(parseEnum[TypeKey](key))

  # meta
  if TypeKey.meta in typ.keys:
    var defaults: Meta
    if typ.parent == nil:
      defaults = Meta(`bit-endian`: be)
    else:
      defaults = Meta(`bit-endian`: typ.parent.meta.`bit-endian`)
    typ.meta = meta(json["meta"], defaults)
  else:
    typ.meta = Meta() # need to do this because meta is an object

  # enums
  if TypeKey.enums in typ.keys:
    typ.enums = initTable[string, OrderedTable[string, int]]()
    for k, v in json["enums"]:
      typ.enums[k] = initOrderedTable[string, int]()
      for i, s in v:
        typ.enums[k][s.getStr] = parseInt(i)

  # types
  if TypeKey.types in typ.keys:
    # Need to construct tree with ids and fill in the rest of the info in a
    # separate step because attributes can reference the ids from the 'type' key
    for key in json["types"].keys:
      let node = Type(id: key.capitalizeAscii, parent: typ)
      typ.types.add(node)
    # This is only possible because Nim's JSON implementation uses OrderedTable
    var i: int
    for _, v in json["types"].pairs:
      toKsTypeRec(typ.types[i], v)
      inc i

  # seq
  if TypeKey.seq in typ.keys:
    for a in json["seq"].items:
      typ.seq.add(field(fkAttr, a["id"].getStr, typ, a))

  # instances
  if TypeKey.instances in typ.keys:
    for k, v in json["instances"].pairs:
      typ.instances.add(field(fkInst, k, typ, v))

  # doc
  if TypeKey.doc in typ.keys:
    typ.doc = json["doc"].getStr

  # doc-ref
  if TypeKey.`doc-ref` in typ.keys:
    typ.`doc-ref` = json["doc-ref"].getStr

  # params
  typ.params = json.getOrDefault("params") # XXX

proc toKsType*(json: JsonNode): Type =
  if not json.hasKey("meta"):
    raise newException(KaitaiError, "Top level type has no 'meta' section")

  if not json["meta"].hasKey("id"):
    raise newException(KaitaiError, "No id in 'meta' section for top level type")

  result = Type(id: json["meta"]["id"].getStr.capitalizeAscii)
  toKsTypeRec(result, json)
  determineImpureSubStructs(result)

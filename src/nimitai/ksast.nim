import macros, json, strutils, strformat
import regex
import exprlang

type
  TypeKey* {.pure.} = enum
    id, types, meta, doc, `doc-ref`, params, seq, instances, enums
  Type* = ref object
    keys*: set[TypeKey]
    parent*: Type
    id*: string
    types*: seq[Type]
    meta*: Meta
    doc*: string
    `doc-ref`*: string
    params*: JsonNode # XXX
    seq*: seq[Field]
    instances*: seq[Field]
    enums*: JsonNode # XXX
  EndianKind* {.pure.} = enum
    le, be
  MetaKey* {.pure.} = enum
    id, title, application, `file-extension`, xref, license, `ks-version`,
    `ks-debug`, `ks-opaque-types`, imports, encoding, endian
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
  RepeatKind* {.pure.} = enum
    none, eos, expr, until
  FieldKey* {.pure.} = enum
    id, doc, `doc-ref`, contents, `type`, repeat, `repeat-expr`, `repeat-until`,
    `if`, size, `size-eos`, process, `enum`, encoding, terminator, consume,
    `include`, `eos-error`, pos, io, value
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
    parent: Type
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
  KaitaiError* = object of Defect

proc hierarchy*(typ: Type): string =
  var
    it = typ
    stack: seq[string]

  while it != nil:
    stack.add(it.id)
    it = it.parent

  while stack != @[]:
    result &= pop(stack)

proc typeLookup(ksType: string, typ: Type): NimNode =
  for st in typ.types:
    if ksType == st.id:
      return ident(hierarchy(st))
  if ksType == typ.id:
    return ident(hierarchy(typ))
  if typ.parent == nil:
    raise newException(KaitaiError, fmt"Type '{ksType}' not found")
  typeLookup(ksType, typ.parent)

proc nativeType(ksType: string, typ: Type): NimNode =
  case ksType
  of "b1": result = ident"bool"
  of "u1": result = ident"uint8"
  of "s1": result = ident"int8"
  of "u2", "u2le", "u2be": result = ident"uint16"
  of "s2", "s2le", "s2be": result = ident"int16"
  of "u4", "u4le", "u4be": result = ident"uint32"
  of "s4", "s4le", "s4be": result = ident"int32"
  of "u8", "u8le", "u8be": result = ident"uint64"
  of "s8", "s8le", "s8be": result = ident"int64"
  of "f4", "f4be", "f4le": result = ident"float32"
  of "f8", "f8be", "f8le": result = ident"float64"
  of "str", "strz": result = ident"string"
  elif ksType.match(re"b[2-9]|b[1-9][0-9]*"):
    result = ident"uint64"
  else:
    result = typeLookup(ksType.capitalizeAscii, typ)

proc ksAsJsonToNim(json: JsonNode, context: string): NimNode =
  case json.kind
  of JString:
    result = expr(json.getStr, context)
  of JInt:
    result = newLit(json.getInt)
  of JBool:
    result = newLit(json.getBool)
  of JNull:
    result = newNilLit()
  else: discard # Should not occur

# XXX very sloppy implementation
proc inferType(expr: NimNode, context: Type): NimNode =
  case expr.kind
  of nnkCharLit:
    result = ident"chat"
  of nnkIntLit:
    result = ident"int"
  of nnkFloatLit:
    result = ident"float"
  of nnkStrLit:
    result = ident"string"
  of nnkSym:
    result = ident"bool"
  of nnkIdent:
    # XXX if eqIdent(expr, "reverse"):
    # XXX if eqIdent(expr, "min"):
    # XXX if eqIdent(expr, "max"):
    # XXX if eqIdent(expr, "first"):
    # XXX if eqIdent(expr, "last"):
    if eqIdent(expr, "to_s"):
      return ident"string"
    if eqIdent(expr, "to_i"):
      return ident"int"
    if eqIdent(expr, "length"):
      return ident"int"
    if eqIdent(expr, "substring"):
      return ident"string"
    if eqIdent(expr, "size"):
      return ident"int"
    if eqIdent(expr, "first"):
      return ident"byte"
    if eqIdent(expr, "last"):
      return ident"byte"
    #for a in context.seq:
    #  if eqIdent(expr, a.id):
    #    return a.`type`.parsed
    #for i in context.instances:
    #  if eqIdent(expr, i.id):
    #    return i.`type`.parsed
    quit(fmt"Identifier {repr(expr)} not found")
  of nnkInfix, nnkPrefix:
    result = inferType(expr[2], context)
  of nnkDotExpr:
    result = inferType(expr[1], context)
  # of nnkNilLit:
  # of nnkCall:
  of nnkBracket:
    result = inferType(expr[0], context)
  of nnkBracketExpr:
    result = inferType(expr[0], context)
  else:
    quit(fmt"Unexpected NimNodeKind '{expr.kind}' during type inference")

proc meta(json: JsonNode): Meta =
  result = Meta()

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

proc field(kind: FieldKind, id: string, parent: Type, json: JsonNode): Field =
  result = Field(kind: kind, id: id, parent: parent)

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
    t = nativeType(ts, result.parent)
  if FieldKey.repeat in result.keys:
    t = nnkBracketExpr.newTree(ident"seq", t)
  if FieldKey.value in result.keys:
    let v = ksAsJsonToNim(json["value"], context) # XXX this is evaluated twice
    t = inferType(v, result.parent)
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

  # XXX enum

  # encoding
  if FieldKey.encoding in result.keys:
    result.encoding = json["encoding"].getStr

  # XXX pad-right

  # XXX terminator

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

  # types
  if TypeKey.types in typ.keys:
    # Need to construct tree with ids and fill in the rest of the info in a
    # separate step because attributes can reference the ids from the 'type' key
    for k, v in json["types"].pairs:
      let node = Type(id: k.capitalizeAscii, parent: typ)
      typ.types.add(node)
    # This is only possible because Nim's JSON implementation uses OrderedTable
    var i: int
    for _, v in json["types"].pairs:
      toKsTypeRec(typ.types[i], v)
      inc i

  # meta
  if TypeKey.meta in typ.keys:
    typ.meta = meta(json["meta"])
  else:
    typ.meta = Meta() # need to do this because meta is an object

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

  # enums
  typ.enums = json.getOrDefault("enums") # XXX

proc toKsType*(json: JsonNode): Type =
  if not json.hasKey("meta"):
    raise newException(KaitaiError, "Top level type has no 'meta' section")

  if not json["meta"].hasKey("id"):
    raise newException(KaitaiError, "No id in 'meta' section for top level type")

  result = Type(id: json["meta"]["id"].getStr.capitalizeAscii)
  toKsTypeRec(result, json)

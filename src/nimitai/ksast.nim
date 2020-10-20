import macros, json, strutils, strformat
import exprlang

type
  TypeKey* {.pure.} = enum
    id, types, meta, doc, `doc-ref`, params, seq, instances, enums
  Type* = ref object
    keys*: set[TypeKey]
    supertype*: Type
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
  KaitaiError* = ref object of Defect

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
    for a in context.seq:
      if eqIdent(expr, a.id):
        return a.`type`.parsed
    for i in context.instances:
      if eqIdent(expr, i.id):
        return i.`type`.parsed
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

proc field(kind: FieldKind, id: string, inType: Type, json: JsonNode): Field =
  result = Field(kind: kind, id: id)

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

  # type
  var
    ts: string
    t: NimNode
  if FieldKey.`type` in result.keys:
    ts = json["type"].getStr
    t = nativeType(ts)
  else:
    ts = ""
    t = nnkBracketExpr.newTree(ident"seq", ident"byte")
  if FieldKey.repeat in result.keys:
    result.`type` = (nnkBracketExpr.newTree(ident"seq", t), ts)
  else:
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
    result.`type` = (inferType(result.value, inType), "")

proc toKsType*(json: JsonNode): Type =
  result = Type()

  # keys
  for key in json.keys:
    result.keys.incl(parseEnum[TypeKey](key))

  # id - Otherwise it will be set by the parent type
  if json.hasKey("meta") and json["meta"].hasKey("id"):
    result.id = json["meta"]["id"].getStr.capitalizeAscii

  # types
  if TypeKey.types in result.keys:
    for k, v in json["types"].pairs:
      let node = v.toKsType
      node.id = k.capitalizeAscii
      node.supertype = result
      result.types.add(node)

  # meta
  if TypeKey.meta in result.keys:
    result.meta = meta(json["meta"])
  else:
    result.meta = Meta() # need to do this because meta is an object

  # seq
  if TypeKey.seq in result.keys:
    for a in json["seq"].items:
      result.seq.add(field(fkAttr, a["id"].getStr, result, a))

  # instances
  if TypeKey.instances in result.keys:
    for k, v in json["instances"].pairs:
      result.instances.add(field(fkInst, k, result, v))

  # doc
  if TypeKey.doc in result.keys:
    result.doc = json["doc"].getStr

  # doc-ref
  if TypeKey.`doc-ref` in result.keys:
    result.`doc-ref` = json["doc-ref"].getStr

  # params
  result.params = json.getOrDefault("params") # XXX

  # enums
  result.enums = json.getOrDefault("enums") # XXX

import macros, json, strutils, runtime

type
  TypeKey* {.pure.} = enum
    id, types, meta, doc, `doc-ref`, params, seq, instances, enums
  Type* = ref object
    set: set[TypeKey]
    supertype*: Type
    id*: string
    types*: seq[Type]
    meta*: Meta
    doc*: string
    `doc-ref`*: string
    params*: JsonNode
    seq*: seq[Attr]
    instances*: seq[Attr]
    enums*: JsonNode
  EndianKind = enum
    ekLe, ekBe
  MetaKey* {.pure.} = enum
    id, title, application, `file-extension`, xref, license, `ks-version`,
    `ks-debug`, `ks-opaque-types`, imports, encoding, endian
  Meta = ref object
    set*: set[MetaKey]
    id*: string
    title*: string
    application*: seq[string]
    `file-extension`*: seq[string]
    xref*: JsonNode
    license*: string
    `ks-version`*: string
    `ks-debug`*: bool
    `ks-opaque-types`*: bool
    imports*: seq[string]
    encoding*: string
    endian*: EndianKind
  repeatKind* = enum
    rkEos, rkExpr, rkUntil
  AttrKey* {.pure.} = enum
    id, doc, `doc-ref`, contents, `type`, repeat, `repeat-expr`, `repeat-until`,
    `if`, size, `size-eos`, process, `enum`, encoding, terminator, consume,
    `include`, `eos-error`, pos, io, value
  Attr* = ref object
    set*: set[AttrKey]
    id*: string
    doc*: string
    `doc-ref`*: string
    contents*: seq[byte]
    `type`*: NimNode
    repeat*: repeatKind
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
    pos*: int
    io*: KaitaiStream
    value*: NimNode

proc attrKeySet*(json: JsonNode): set[AttrKey] =
  for key in json.keys:
    result.incl(parseEnum[AttrKey](key))

proc toKsType*(json: JsonNode): Type =
  result = Type()
  for key in json.keys:
    result.set.incl(parseEnum[TypeKey](key))
  if json.hasKey("meta") and json["meta"].hasKey("id"):
    result.id = json["meta"]["id"].getStr.capitalizeAscii
  if TypeKey.meta in result.set:
    var meta = Meta()
    for key in json["meta"].keys:
      meta.set.incl(parseEnum[MetaKey](key))
    if MetaKey.id in meta.set:
      meta.id = json["meta"]["id"].getStr
    if MetaKey.title in meta.set:
      meta.title = json["meta"]["title"].getStr
    if MetaKey.application in meta.set:
      case meta.application.kind
      of JArray:
        for s in json["meta"]["application"].items:
          meta.application.add(s)
      of JString:
          meta.application.add(json["meta"]["application"].getStr)
      else discard # should not occur
    if MetaKey.`file-extension` in meta.set:
      discard # XXX
    if MetaKey.xref in meta.set:
      discard # XXX
    if MetaKey.license in meta.set:
      discard # XXX
    if MetaKey.`ks-version` in meta.set:
      discard # XXX
    if MetaKey.`ks-debug` in meta.set:
      discard # XXX
    if MetaKey.`ks-opaque-types` in meta.set:
      discard # XXX
    if MetaKey.imports in meta.set:
      discard # XXX
    if MetaKey.encoding in meta.set:
      discard # XXX
    if MetaKey.endian in meta.set:
      discard # XXX
    if MetaKey.id in meta.set:
      discard # XXX
    if MetaKey.id in meta.set:
      discard # XXX
    if MetaKey.id in meta.set:
      discard # XXX
    result.meta = meta
  result.params = json.getOrDefault("params") # XXX
  result.enums = json.getOrDefault("enums")) # XXX
  # XXX doc
  # XXX `doc-ref`
  # XXX seq
  # XXX instances
  if TypeKey.types in result.set:
    for k, v in json["types"].pairs:
      let node = v.toKsType
      node.id = k.capitalizeAscii
      node.supertype = result
      result.types.add(node)

# For debugging
proc `$`*(node: Type): string =
  result &= "name: " & node.id & "\n"

  result &= "parent: "
  if node.supertype != nil:
    result &= $node.supertype.id
  else:
    result &= "nil"
  result &= "\n"

  result &= "children: "
  if node.types.len != 0:
    result &= "\n"
    for c in node.types:
      result &= "  " & c.id & "\n"
  else:
    result &= "nil"
  result &= "\n"

  result &= "meta: "
  if node.meta != nil:
    result &= $node.meta
  else:
    result &= "nil"
  result &= "\n"

  result &= "doc: "
  if node.doc != nil:
    result &= $node.doc
  else:
    result &= "nil"
  result &= "\n"

  result &= "doc-ref: "
  if node.`doc-ref` != nil:
    result &= $node.`doc-ref`
  else:
    result &= "nil"
  result &= "\n"

  result &= "params: "
  if node.params != nil:
    result &= $node.params
  else:
    result &= "nil"
  result &= "\n"

  result &= "seq: "
  if node.seq != nil:
    result &= $node.seq
  else:
    result &= "nil"
  result &= "\n"

  result &= "instances: "
  if node.instances != nil:
    result &= $node.instances
  else:
    result &= "nil"
  result &= "\n"

  result &= "enums: "
  if node.enums != nil:
    result &= $node.enums
  else:
    result &= "nil"
  result &= "\n"

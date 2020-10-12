import macros, json, strutils, kaitai_struct_nim_runtime

type
  TypeKey* {.pure.} = enum
    id, types, meta, doc, `doc-ref`, params, seq, instances, enums
  Type* = ref object
    set: set[TypeKey]
    supertype*: Type
    id*: string
    types*: seq[Type]
    meta*: JsonNode
    doc*: JsonNode
    `doc-ref`*: JsonNode 
    params*: JsonNode 
    seq*: JsonNode 
    instances*: JsonNode
    enums*: JsonNode
  repeatKind = enum
    rkEos
    rkExpr
    rkUntil
  AttrKey* {.pure.} = enum
    id, doc, `doc-ref`, contents, `type`, repeat, `repeat-expr`, `repeat-until`,
    `if`, size, `size-eos`, process, `enum`, encoding, terminator, consume,
    `include`, `eos-error`, pos, io, value
  Attr* = ref object
    set: set[AttrKey]
    id: string
    doc: string
    `doc-ref`: string
    contents: seq[byte]
    `type`: NimNode
    repeat: repeatKind
    `repeat-expr`: NimNode
    `repeat-until`: NimNode
    `if`: NimNode
    size: NimNode
    `size-eos`: bool
    process: proc()
    `enum`: string
    encoding: string
    `pad-right`: byte
    terminator: byte
    consume: bool
    `include`: bool
    `eos-error`: bool
    pos: int
    io: KaitaiStream
    value: NimNode

proc attrKeySet*(json: JsonNode): set[AttrKey] =
  for key in json.keys:
    result.incl(parseEnum[AttrKey](key))

proc toKsType*(json: JsonNode): Type =
  result = Type(
    meta: json.getOrDefault("meta"),
    doc: json.getOrDefault("doc"),
    `doc-ref`: json.getOrDefault("doc-ref"),
    params: json.getOrDefault("params"),
    seq: json.getOrDefault("seq"),
    instances: json.getOrDefault("instances"),
    enums: json.getOrDefault("enums"))
  if json.hasKey("meta") and json["meta"].hasKey("id"):
    result.id = json["meta"]["id"].getStr.capitalizeAscii
  if json.hasKey("types"):
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

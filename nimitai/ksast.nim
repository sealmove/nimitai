import json, strutils

type KsNode* = ref object
  name*: string
  parent*: KsNode
  children*: seq[KsNode]
  meta*: JsonNode
  doc*: JsonNode
  `doc-ref`*: JsonNode 
  params*: JsonNode 
  seq*: JsonNode 
  instances*: JsonNode
  enums*: JsonNode

proc toKsNode*(json: JsonNode): KsNode =
  result = KsNode(
    meta: json.getOrDefault("meta"),
    doc: json.getOrDefault("doc"),
    `doc-ref`: json.getOrDefault("doc-ref"),
    params: json.getOrDefault("params"),
    seq: json.getOrDefault("seq"),
    instances: json.getOrDefault("instances"),
    enums: json.getOrDefault("enums"))
  if json.hasKey("meta") and json["meta"].hasKey("id"):
    result.name = json["meta"]["id"].getStr.capitalizeAscii
  if json.hasKey("types"):
    for k, v in json["types"].pairs:
      let node = v.toKsNode
      node.name = k.capitalizeAscii
      node.parent = result
      result.children.add(node)

# For debugging
proc `$`*(node: KsNode): string =
  result &= "name: " & node.name & "\n"

  result &= "parent: "
  if node.parent != nil:
    result &= $node.parent.name
  else:
    result &= "nil"
  result &= "\n"

  result &= "children: "
  if node.children.len != 0:
    result &= "\n"
    for c in node.children:
      result &= "  " & c.name & "\n"
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

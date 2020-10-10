import nimitai/ksast
import json, macros

const
  rootTypeName = "KaitaiStruct"

proc nativeType(ksyType: string): string =
  case ksyType
  of "u1": result = "uint8"
  of "s1": result = "int8"
  of "u2", "u2le", "u2be": result = "uint16"
  of "s2", "s2le", "s2be": result = "int16"
  of "u4", "u4le", "u4be": result = "uint32"
  of "s4", "s4le", "s4be": result = "int32"
  of "u8", "u8le", "u8be": result = "uint64"
  of "s8", "s8le", "s8be": result = "int64"
  of "f4", "f4be", "f4le": result = "float32"
  of "f8", "f8be", "f8le": result = "float64"
  of "str", "strz": result = "string"
  else: # TODO: implement look-up here
    result = ksyType

proc attribute(json: JsonNode): NimNode =
  newIdentDefs(
    ident(json["id"].getStr),
    ident(nativeType(json["type"].getStr)))

proc type(node: KsNode): NimNode =
  var fields = newTree(nnkRecList)

  fields.add(
    newIdentDefs(
      ident"parent",
      ident(rootTypeName)))

  for a in node.`seq`.items:
    fields.add(attribute(a))

  result = nnkTypeDef.newTree(
    ident(node.name),
    newEmptyNode(),
    nnkRefTy.newTree(
      nnkObjectTy.newTree(
        newEmptyNode(),
        nnkOfInherit.newTree(
          ident(rootTypeName)),
        fields)))

proc typeSection(node: KsNode): NimNode =
  result = newTree(nnkTypeSection)
  result.add(type(node))
  for t in node.children:
    result.add(type(t))

proc generateParser*(spec: JsonNode): NimNode =
  let spec = spec.toKsNode
  result = newStmtList(
    typeSection(spec))

# static library
macro injectParser*(spec: static[JsonNode]) =
  generateParser(spec)

# dynamic library
proc createDynlib*(spec: JsonNode, path: string) = discard

# source code
proc outputModule*(spec: JsonNode): string = discard

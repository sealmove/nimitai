import nimitai/[ksast, exprlang]
import json, macros, regex, strutils

const
  rootTypeName = "KaitaiStruct"
  streamTypeName = "KaitaiStream"

type
  attrKey {.pure.} = enum
    id, doc, `doc-ref`, contents, `type`, repeat, `repeat-expr`, `repeat-until`,
    `if`, size, `size-eos`, process, `enum`, encoding, terminator, consume,
    `include`, `eos-error`, pos, io, value

proc attrKeySet(json: JsonNode): set[attrKey] =
  for key in json.keys:
    result.incl(parseEnum[attrKey](key))

proc nativeType(ksyType: string): string =
  case ksyType
  of "u1": result = "uint8"
  of "s1": result = "int8"
  of "u2": result = "uint16"
  of "s2": result = "int16"
  of "u4": result = "uint32"
  of "s4": result = "int32"
  of "u8": result = "uint64"
  of "s8": result = "int64"
  of "f4": result = "float32"
  of "f8": result = "float64"
  of "str", "strz": result = "string"
  else: # TODO: implement look-up here
    result = ksyType.capitalizeAscii

proc parentType(node: KsNode): string =
  if node.parent == nil: rootTypeName else: node.parent.name

proc attr(json: JsonNode): NimNode =
  newIdentDefs(
    ident(json["id"].getStr),
    ident(nativeType(json["type"].getStr)))

proc type(node: KsNode): NimNode =
  var fields = newTree(nnkRecList)

  fields.add(
    newIdentDefs(
      ident"parent",
      ident(parentType(node))))

  for a in node.seq:
    fields.add(attr(a))

  result = nnkTypeDef.newTree(
    ident(node.name),
    newEmptyNode(),
    nnkRefTy.newTree(
      nnkObjectTy.newTree(
        newEmptyNode(),
        nnkOfInherit.newTree(
          ident(rootTypeName)),
        fields)))

proc parseAttrExprUser(json: JsonNode, fromRaw: bool): NimNode =
  let
    t = json["type"].getStr.capitalizeAscii
    io = if fromRaw: ident(t & "Raw")
         else: newDotExpr(ident"result", ident"io")
  result = newCall(
    newDotExpr(
      ident(t),
      ident"read"),
    io,
    newDotExpr(
      ident"result",
      ident"root"),
    ident"result")

# A series of assignments of parsing calls to local variables or object fields
proc parseAttr(json: JsonNode): NimNode =
  result = newStmtList()
  let
    s = attrKeySet(json)
    id = json["id"].getStr

  if attrKey.`type` in s:
    let t = json["type"].getStr

    # Integer type
    if t.match(re"[us][1248]|f[48]"):
      var procName = "read" & t
      if not t.match(re"[us][1]"):
        procName &= "le" # XXX
      result.add(
        newAssignment(
          newDotExpr(
            ident"result",
            ident(json["id"].getStr)),
            newCall(
              procName,
              newDotExpr(
                ident"result",
                ident"io"))))
    # User-defined type
    else:
      let userT = t.capitalizeAscii
      if attrKey.`size` in s:
        let
          size = expr(json["size"].getStr)
          raw = ident(id & "Raw")
          substream = ident(id & "Io")
        result.add(
          newLetStmt(
            raw,
            newCall(
              newDotExpr(
                newDotExpr(
                  ident"result",
                  ident"io"),
                ident"readBytes"),
              newCall(
                ident"int",
                newDotExpr(
                  ident"result",
                  size)))))
        result.add(
          newLetStmt(
            substream,
            newCall(
              ident"newKaitaiStream",
              raw)))
        result.add(
          newAssignment(
            newDotExpr(
              ident"result",
              ident(id)),
            newCall(
              newDotExpr(
                ident(userT),
                ident"read"),
              substream,
              newDotExpr(
                ident"result",
                ident"root"),
              ident"result")))

proc typeSection(node: KsNode): NimNode =
  result = newTree(nnkTypeSection)
  result.add(type(node))
  for t in node.children:
    result.add(type(t))

proc readProc(node: KsNode): NimNode =
  result = newProc(name = ident"read")

  result.params = nnkFormalParams.newTree(
    ident(node.name),
    newIdentDefs(
      ident"_",
      nnkBracketExpr.newTree(
        ident"typedesc",
        ident(node.name))),
    newIdentDefs(
      ident"io",
      ident(streamTypeName)),
    newIdentDefs(
      ident"root",
      ident(rootTypeName),
      newNilLit()),
    newIdentDefs(
      ident"parent",
      ident(parentType(node)),
      newNilLit()))

  var parseAttrs = newStmtList()
  for a in node.seq:
    parseAttrs.add(parseAttr(a))

  result.body = newStmtList(
    newAssignment(
      ident"result",
      nnkObjConstr.newTree(
        ident(node.name),
        newColonExpr(
          ident"io",
          ident"io"),
        newColonExpr(
          ident"parent",
          ident"parent"))),
    newAssignment(
      newDotExpr(
        ident"result",
        ident"root"),
      nnkIfExpr.newTree(
        nnkElifExpr.newTree(
          infix(
            ident"root",
            "==",
            newNilLit()),
          ident"result"),
        nnkElseExpr.newTree(
          ident"root"))),
    parseAttrs)

proc readProcs(node: KsNode): NimNode =
  result = newStmtList()
  if node.children != @[]:
    for c in node.children:
      result.add(readProcs(c))
  result.add(readProc(node))

proc fromFileProc(node: KsNode): NimNode =
  newStmtList(
    nnkProcDef.newTree(
      ident"fromFile",
      newEmptyNode(),
      newEmptyNode(),
      nnkFormalParams.newTree(
        ident(node.name),
        newIdentDefs(
          ident"_",
          nnkBracketExpr.newTree(
            ident"typedesc",
            ident(node.name))),
        newIdentDefs(
          ident"filename",
          ident"string")),
      newEmptyNode(),
      newEmptyNode(),
      newStmtList(
        newCall(
          ident"read",
          ident(node.name),
          newCall(
            ident"newKaitaiFileStream",
            ident"filename")))))

proc fromFileProcs(node: KsNode): NimNode =
  result = newStmtList()
  if node.children != @[]:
    for c in node.children:
      result.add(fromFileProcs(c))
  result.add(fromFileProc(node))

proc generateParser*(spec: JsonNode): NimNode =
  let spec = spec.toKsNode
  result = newStmtList(
    typeSection(spec),
    readProcs(spec),
    fromFileProcs(spec))
  echo repr result

# static library
macro injectParser*(spec: static[JsonNode]) =
  generateParser(spec)

# dynamic library
proc createDynlib*(spec: JsonNode, path: string) = discard

# source code
proc outputModule*(spec: JsonNode): string = discard

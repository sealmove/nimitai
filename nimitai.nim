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

proc nativeType(ksyType: string): NimNode =
  case ksyType
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
  elif ksyType.match(re"b[2-9]|b[1-9][0-9]*"):
    result = ident"uint64"
  else:
    # TODO: implement look-up here
    result = ident(ksyType.capitalizeAscii)

proc inferType(json: JsonNode): NimNode =
  ident"int"

proc parentType(node: KsNode): string =
  if node.parent == nil: rootTypeName else: node.parent.name

proc attrDecl(json: JsonNode): NimNode =
  newIdentDefs(
    ident(json["id"].getStr),
    nativeType(json["type"].getStr))

proc instType(attr: JsonNode): NimNode =
  if attr.hasKey("type"):
    nativeType(attr["type"].getStr)
  else:
    inferType(attr["value"])

proc instDecl(key: string, value: JsonNode): NimNode =
  let t = instType(value)

  newIdentDefs(
    ident(key & "Inst"),
    nnkBracketExpr.newTree(
      ident"Option",
      t))

proc typeDecl(node: KsNode): NimNode =
  var fields = newTree(nnkRecList)

  fields.add(
    newIdentDefs(
      ident"parent",
      ident(parentType(node))))

  if node.seq != nil:
    for a in node.seq:
      fields.add(attrDecl(a))

  if node.instances != nil:
    for k, v in node.instances.pairs:
      fields.add(instDecl(k, v))

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

  # This should be used for both size and sizeless attributes
  var stream, size: NimNode

  # Size key means we get a substream
  if attrKey.`size` in s:
    stream = ident(id & "Io")
    size = expr(json["size"].getStr)
    let raw = ident(id & "Raw")
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
        stream,
        newCall(
          ident"newKaitaiStream",
          raw)))

  if attrKey.`type` in s:
    let t = json["type"].getStr

    # Number
    if t.match(re"([us][1248]|f[48])(be|le)?"):
      var procName = "read" & t
      if not t.match(re"([us][1])|(.*(be|le))"):
        procName &= "le" # XXX
      result.add(
        newAssignment(
          newDotExpr(
            ident"result",
            ident(id)),
            newCall(
              procName,
              newDotExpr(
                ident"result",
                ident"io"))))

    # Bool
    elif t == "b1":
      result.add(
        newAssignment(
          newDotExpr(
            ident"result",
            ident(id)),
          newCall(
            ident"bool",
            newCall(
              "readBitsIntBe",
              newDotExpr(
                ident"result",
                ident"io"),
              newLit(1)))))

    # Number from bits
    elif t.match(re"b[2-9]|b[1-9][0-9]*"):
      let bits = parseInt(t[1..^1])
      result.add(
        newAssignment(
          newDotExpr(
            ident"result",
            ident(id)),
          newCall(
            "readBitsIntBe",
            newDotExpr(
              ident"result",
              ident"io"),
            newLit(bits))))

    # User-defined type
    else:
      result.add(
        newAssignment(
          newDotExpr(
            ident"result",
            ident(id)),
          newCall(
            newDotExpr(
              ident(t.capitalizeAscii),
              ident"read"),
            stream,
            newDotExpr(
              ident"result",
              ident"root"),
            ident"result")))

  # Typeless
  else:
    result.add(
      newAssignment(
        newDotExpr(
          ident"result",
          ident(id)),
        newDotExpr(
          stream,
          newCall(
            ident"readBytes",
            size))))

proc typeSection(node: KsNode): NimNode =
  result = newTree(nnkTypeSection)
  result.add(typeDecl(node))
  for t in node.children:
    result.add(typeDecl(t))

proc instanceProc(attrName, objName: string; attr: JsonNode): NimNode =
  let inst = ident(attrName & "Inst")
  result = newProc(
    ident(attrName),
    @[instType(attr),
      newIdentDefs(
        ident"this",
        ident(objName))])
  result.body = newStmtList(
    newIfStmt(
      (newCall(
        ident"isNone",
        newDotExpr(
          ident"this",
          inst)),
       newAssignment(
         newDotExpr(
           ident"this",
           inst),
         newCall(
           ident"some",
           newLit(attr["value"].getInt))))),
    nnkReturnStmt.newTree(
      newCall(
        ident"get",
        newDotExpr(
          ident"this",
          inst))))

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

proc procs(node: KsNode): NimNode =
  result = newStmtList()
  if node.children != @[]:
    for c in node.children:
      result.add(procs(c))
  if node.instances != nil:
    for k, v in node.instances.pairs:
      result.add(instanceProc(k, node.name, v))
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
    procs(spec),
    fromFileProcs(spec))

# static library
macro injectParser*(spec: static[JsonNode]) =
  generateParser(spec)

# dynamic library
proc createDynlib*(spec: JsonNode, path: string) = discard

# source code
proc outputModule*(spec: JsonNode): string = discard

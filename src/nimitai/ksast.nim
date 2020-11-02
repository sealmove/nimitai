import macros, json, strutils, tables, sequtils, strformat
import types, exprlang

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

proc toNim*(ksType: KsType): NimNode =
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
    result = nnkBracketExpr.newTree(ident"seq", toNim(ksType.elemtype))
  of ktkUser:
    result = ident(buildNimTypeId(ksType.usertype))
  of ktkEnum:
    result = ident(buildNimTypeId(ksType.owningtype) & ksType.enumname)
  of ktkStream:
    result = ident(streamTypeName)

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

proc jsonToExpr(json: JsonNode, typ: Type): KsNode =
  case json.kind
  of JString:
    result = json.getStr.toKs(typ)
  of JInt:
    result = KsNode(kind: knkInt, intval: json.getInt, cx: typ)
  of JFloat:
    result = KsNode(kind: knkFloat, floatval: json.getFloat, cx: typ)
  of JBool:
    result = KsNode(kind: knkBool, boolval: json.getBool, cx: typ)
  else: discard # Should not occur

proc inferType(node: KsNode): KsType =
  case node.kind
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
    case node.strval
    of "_io":
      result = tstream()
    else:
      result = node.cx.access(node.strval)
  of knkEnum:
    result = tenum(node.cx, node.enumscope)
  of knkArr:
    let types = node.sons.mapIt(inferType(it))
    case types[0].kind
    of ktkUInt, ktkSInt:
      var
        bytes: int
        signed: bool
      for t in types:
        bytes = t.bytes
        if t.kind == ktkSInt:
          signed = true
      if signed:
        result = tarr(tsint(bytes))
      else:
        result = tarr(tuint(bytes))
    else:
      result = tarr(types[0])
  of knkMeth:
    doAssert node.sons[0].kind == knkId
    case node.sons[0].strval
    of "to_s":
      result = tstr()
    of "to_i":
      result = tsint(8)
    of "length":
      result = tsint(8)
    of "substring":
      result = tstr()
    of "size":
      result = tsint(8)
    of "eof":
      result = tuint(1)
    else:
      quit(fmt"Method {node.sons[0].strval} not found")
  of knkIdx:
    let x = inferType(node.sons[0])
    doAssert x.kind == ktkArr
    result = x.elemtype
  of knkCast:
    result = node.kstype
  of knkDotExpr:
    let lefttype = inferType(node.sons[0])
    case node.sons[1].kind
    of knkId:
      result = access(lefttype.usertype, node.sons[1].strval)
    of knkMeth:
      case node.sons[1].sons[0].strval
      of "min", "max", "first", "last", "reverse":
        result = inferType(node.sons[0])
      else:
        result = inferType(node.sons[1])
    else: discard # XXX
  of knkUnary:
    result = inferType(node.sons[1])
  of knkInfix:
    let (l, r) = (inferType(node.sons[0]),
                  inferType(node.sons[2]))
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
    let (c, t, f) = (inferType(node.sons[0]),
                     inferType(node.sons[1]),
                     inferType(node.sons[2]))
    doAssert (c.kind == ktkBit and c.bits == 1)
    doAssert t.kind == f.kind
    result = t

proc toNim*(node: KsNode): NimNode =
  case node.kind
  of knkBool:
    result = newLit(node.boolval)
  of knkInt:
    result = newIntLitNode(node.intval)
  of knkFloat:
    result = newFloatLitNode(node.floatval)
  of knkStr:
    result = newStrLitNode(node.strval)
  of knkOp:
    case node.strval
    of "<<": result = ident"shl"
    of ">>": result = ident"shr"
    of "&" : result = ident"and"
    of "|" : result = ident"or"
    of "^" : result = ident"xor"
    of "/" : result = ident"ksdiv" # depends on runtime
    of "%" : result = ident"ksmod" # depends on runtime
    else   : result = ident(node.strval)
  of knkId: # XXX
    case node.strval
    of "null":
      result = ident"nil"
    of "_parent":
      result = newDotExpr(ident"this", ident"parent")
    of "_root":
      result = newDotExpr(ident"this", ident"root")
    of "_io":
      result = newDotExpr(ident"this", ident"io")
    of "_":
      result = ident"x"
    else:
      result = newDotExpr(ident"this", ident(node.strval))
  of knkEnum: # XXX implement relative matching
    if node.cx != nil:
      result = newDotExpr(
        ident(matchAndBuildEnum(node.enumscope, node.cx)),
        ident(node.enumval))
    else: # needed for generating tests
      result = newDotExpr(
        ident(join(node.enumscope).capitalizeAscii),
        ident(node.enumval))
  of knkCast:
    result = node.kstype.toNim
  of knkArr:
    # Need to use more fine-grained integer type because conversion is not auto
    if node.sons == @[]:
      return nnkBracketExpr.newTree(ident"seq", ident"byte")
    let
      x = newTree(nnkBracket)
      typeOfFirst = inferType(node.sons[0])
    for s in node.sons:
      x.add toNim(s)
    if typeOfFirst.kind in {ktkUInt, ktkSInt}:
      var
        bytes: int
        sign: bool
      for son in node.sons:
        let t = inferType(son)
        if t.bytes > bytes: bytes = t.bytes
        if t.kind == ktkSInt: sign = true
      case sign
      of true:
        case bytes
        of 1: x[0] = newCall(ident"int8", x[0])
        of 2: x[0] = newCall(ident"int16", x[0])
        of 4: x[0] = newCall(ident"int32", x[0])
        of 8: x[0] = newCall(ident"int64", x[0])
        else: discard
      of false:
        case bytes
        of 1: x[0] = newCall(ident"uint8", x[0])
        of 2: x[0] = newCall(ident"uint16", x[0])
        of 4: x[0] = newCall(ident"uint32", x[0])
        of 8: x[0] = newCall(ident"uint64", x[0])
        else: discard
    result = prefix(x, "@")
  of knkMeth:
    result = newTree(nnkCall)
    result.add(ident(node.sons[0].strval))
    for i in 1 ..< node.sons.len:
      result.add(toNim(node.sons[i]))
  of knkIdx:
    result = nnkBracketExpr.newTree(
      toNim(node.sons[0]),
      toNim(node.sons[1]))
  of knkDotExpr:
    var (l, r) = (node.sons[0], node.sons[1])
    case r.kind
    # ! special case because of Nim AST peculiarity
    of knkMeth:
      r.sons.insert(l, 1)
      result = toNim(r)
    of knkCast:
      result = newCall(
        toNim(r),
        toNim(l))
    of knkId:
      result = newDotExpr(
        toNim(l),
        ident(r.strval))
    of knkIdx:
      result = newDotExpr(
        toNim(l),
        toNim(r))
    else:
      raise newException(
        KaitaiError, fmt"unexpected '{r.kind}' during transpilation")

  of knkUnary:
    result = nnkPrefix.newTree(
      toNim(node.sons[0]),
      toNim(node.sons[1]))
  of knkInfix:
    result = nnkInfix.newTree(
      toNim(node.sons[1]),
      toNim(node.sons[0]),
      toNim(node.sons[2]))
  of knkTernary:
    result = nnkIfStmt.newTree(
      nnkElifBranch.newTree(
        toNim(node.sons[0]),
        toNim(node.sons[1])),
      nnkElse.newTree(toNim(node.sons[2])))

proc meta(json: JsonNode, defaults: Meta): Meta =
  result = defaults

  for key in json.keys:
    result.keys.incl(parseEnum[MetaKey](key))

  if mkId in result.keys:
    result.id = json["id"].getStr

  if mkTitle in result.keys:
    result.title = json["title"].getStr

  if mkApplication in result.keys:
    let jnode = json["application"]
    case jnode.kind
    of JArray:
      for s in jnode.items:
        result.application.add(s.getStr)
    of JString:
      result.application.add(jnode.getStr)
    else: discard # should not occur

  if mkFileExtension in result.keys:
    let jnode = json["file-extension"]
    case jnode.kind
    of JArray:
      for s in jnode.items:
        result.fileExtension.add(s.getStr)
    of JString:
      result.fileExtension.add(jnode.getStr)
    else: discard # should not occur

  if mkXref in result.keys:
    result.xref = json["xref"] # XXX

  if mkLicense in result.keys:
    result.license = json["license"].getStr

  if mkKsVersion in result.keys:
    result.ksVersion = json["ks-version"].getStr

  if mkKsDebug in result.keys:
    result.ksDebug = json["ks-debug"].getBool

  if mkKsOpaqueTypes in result.keys:
    result.ksOpaqueTypes = json["ks-debug"].getBool

  if mkImports in result.keys:
    let jnode = json["imports"]
    case jnode.kind
    of JArray:
      for s in jnode.items:
        result.imports.add(s.getStr)
    of JString:
      result.imports.add(jnode.getStr)
    else: discard # should not occur

  if mkEncoding in result.keys:
    result.encoding = json["encoding"].getStr

  if mkEndian in result.keys:
    result.endian = parseEnum[Endian](json["endian"].getStr)

  if mkBitEndian in result.keys:
    result.bitEndian = parseEnum[Endian](json["bit-endian"].getStr)

proc field(kind: FieldKind, id: string, st: Type, json: JsonNode): Field =
  result = Field(kind: kind, id: id, st: st)

  for key in json.keys:
    result.keys.incl(parseEnum[FieldKey](key))

  if fkDoc in result.keys:
    result.doc = json["doc"].getStr

  if fkDocRef in result.keys:
    result.docRef = json["doc-ref"].getStr

  if fkContents in result.keys:
    let x = json["contents"]
    case x.kind
    of JString:
      let parsed = x.getStr.toKs(st)
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

  if fkRepeat in result.keys:
    result.repeat = parseEnum[RepeatKind](json["repeat"].getStr)

  if fkRepeatExpr in result.keys:
    result.repeatExpr = jsonToExpr(json["repeat-expr"], result.st)

  if fkRepeatUntil in result.keys:
    result.repeatUntil = jsonToExpr(json["repeat-until"], result.st)

  if fkIf in result.keys:
    result.`if` = jsonToExpr(json["if"], result.st)

  if fkSize in result.keys:
    result.size = jsonToExpr(json["size"], result.st)

  if fkSizeEos in result.keys:
    result.sizeEos = json["size-eos"].getBool

  # XXX process

  if fkEnum in result.keys:
    result.`enum` = split(json["enum"].getStr, "::")

  if fkEncoding in result.keys:
    result.encoding = json["encoding"].getStr

  if fkPadRight in result.keys:
    result.padRight = jsonToByte(json["pad-right"])

  if fkTerminator in result.keys:
    result.terminator = jsonToByte(json["terminator"])

  if fkConsume in result.keys:
    result.consume = json["consume"].getBool
  else:
    result.consume = true

  if fkInclude in result.keys:
    result.`include` = json["include"].getBool

  if fkEosError in result.keys:
    result.eosError = json["eos-error"].getBool
  else:
    result.eosError = true

  if fkIo in result.keys:
    result.io = jsonToExpr(json["io"], st)
  else:
    result.io = KsNode(kind: knkId, strval: "io", cx: st)

  if fkPos in result.keys:
    result.pos = jsonToExpr(json["pos"], st)

  if fkValue in result.keys:
    result.value = jsonToExpr(json["value"], st)
    result.`type` = inferType(result.value)

proc verboseEnum(json: JsonNode): VerboseEnum =
  result = VerboseEnum()
  for key in json.keys:
    result.keys.incl(parseEnum[VerboseEnumKey](key))

  if vekId in result.keys:
    result.id = json["id"].getStr

  if vekDoc in result.keys:
    result.doc = json["doc"].getStr

  if vekDocRef in result.keys:
    result.docRef = json["doc-ref"].getStr

  if vekDocRef in result.keys:
    result.origId = json["-orig-id"].getStr

proc fillType(typ: Type, json: JsonNode) =
  for key in json.keys:
    typ.keys.incl(parseEnum[TypeKey](key))

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

  if tkSeq in typ.keys:
    for a in json["seq"].items:
      typ.seq.add(field(fkAttr, a["id"].getStr, typ, a))

  if tkInstances in typ.keys:
    for k, v in json["instances"].pairs:
      typ.instances.add(field(fkInst, k, typ, v))

  if tkDoc in typ.keys:
    typ.doc = json["doc"].getStr

  if tkDocRef in typ.keys:
    typ.docRef = json["doc-ref"].getStr

 # XXX
  typ.params = json.getOrDefault("params")

proc toType*(json: JsonNode): Type =
  if not json.hasKey("meta"):
    raise newException(KaitaiError, "Top level type has no 'meta' section")

  if not json["meta"].hasKey("id"):
    raise newException(KaitaiError, "No id in 'meta' section for top level type")

  result = Type(id: json["meta"]["id"].getStr.capitalizeAscii)
  fillType(result, json)

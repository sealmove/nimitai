import macros, strutils, nimitai/private/ast
#XXX Need to document necessary imports and pragmas
# imports: kaitai_struct_nim_runtime, options
# pragmas: dotOperators

proc toNimType*(typ: KsType): NimNode =
  case typ.kind
  of ktkBit:
    case typ.bits
    of  1 ..  8: result = ident"uint8"
    of  9 .. 16: result = ident"uint16"
    of 17 .. 32: result = ident"uint32"
    of 33 .. 64: result = ident"uint64"
    else: discard
  of ktkBool:
    result = ident"bool"
  of ktkInt:
    case typ.isSigned
    of false:
      case typ.size
      of 1: result = ident"uint8"
      of 2: result = ident"uint16"
      of 4: result = ident"uint32"
      of 8: result = ident"uint64"
      else: discard
    of true:
      case typ.size
      of 1: result = ident"int8"
      of 2: result = ident"int16"
      of 4: result = ident"int32"
      of 8: result = ident"int64"
      else: discard
  of ktkFloat:
    case typ.precision:
    of 4: result = ident"float32"
    of 8: result = ident"float64"
    else: discard
  of ktkArray: #XXX nested array (recursion)
    result = nnkBracketExpr.newTree(
               ident"seq",
               ident"byte")
  of ktkStr, ktkStrz:
    result = ident"string"
  of ktkUser:
    result = ident(typ.id)

proc toNim*(expr: KsNode): NimNode =
  case expr.kind
  of knkIdentifier:
    result = ident(expr.id)
  of knkLiteral:
    case expr.typ.kind
    of ktkBit:
      let lit = parseBiggestUInt(expr.val)
      case expr.typ.bits
      of  1 ..  8: result = newLit(uint8(lit))
      of  9 .. 16: result = newLit(uint16(lit))
      of 17 .. 32: result = newLit(uint32(lit))
      of 33 .. 64: result = newLit(uint64(lit))
      else: discard
    of ktkBool:
      result = newLit(parseBool(expr.val))
    of ktkInt:
      case expr.typ.radix
      of rHex:
        result = newLit(parseHexInt(expr.val))
      of rBin:
        result = newLit(parseBinInt(expr.val))
      of rDec:
        if expr.typ.isSigned:
          let lit = parseBiggestInt(expr.val)
          case expr.typ.size
          of 1: result = newLit(int8(lit))
          of 2: result = newLit(int16(lit))
          of 4: result = newLit(int32(lit))
          of 8: result = newLit(int64(lit))
          else: discard
        else:
          let lit = parseBiggestUInt(expr.val)
          case expr.typ.size
          of 1: result = newLit(uint8(lit))
          of 2: result = newLit(uint16(lit))
          of 4: result = newLit(uint32(lit))
          of 8: result = newLit(uint64(lit))
          else: discard
    of ktkFloat:
      case expr.typ.precision
      of 4: result = newLit(float32(parseFloat(expr.val)))
      of 8: result = newLit(float64(parseFloat(expr.val)))
      else: discard
    of ktkArray:
      discard
    of ktkStr:
      result = newLit(expr.val)
    of ktkStrz:
      discard
    of ktkUser:
      discard
  of knkArithOp: #XXX
    discard
  of knkBitOp: #XXX
    discard
  of knkCmpOp: #XXX
    discard
  of knkRelOp: #XXX
    discard
  of knkUnaryOp:
    var op: string
    case expr.uo
    of uoMinus : op = "-"
    of uoInvert: op = "not"
    of uoNot   : op = "not"
    result = prefix(expr.uoO.toNim, op)
    discard #XXX

proc parentType(t: Nimitype): NimNode =
  if t.parent == "":
    nnkRefTy.newTree(
      ident"RootObj")
  else:
    ident(t.parent)

proc bits(typ: string): int =
  parseInt(typ[1..^1])

#XXX
proc readField(f: Field, e: Endian): NimNode =
  var letStmt: NimNode
  case f.isLazy
  of false:
    case f.typ.kind
    of ktkBit:
      var sizeAdj: string
      case f.typ.bits
      of  1 ..  8: sizeAdj = "uint8"
      of  9 .. 16: sizeAdj = "uint16"
      of 17 .. 32: sizeAdj = "uint32"
      of 33 .. 64: sizeAdj = "uint64"
      else: discard
      letStmt = newLetStmt(
        ident(f.id),
        newCall(
          ident(sizeAdj),
          newCall(
            ident"readBitsInt",
            ident"io",
            newLit(f.typ.bits))))
    of ktkBool:
      letStmt = newLetStmt(
        ident(f.id),
        newCall(
          ident"bool",
          newCall(
            ident"readBitsInt",
            ident"io",
            newLit(1))))
    of ktkInt:
      var fn = "read"
      if f.typ.isSigned: fn &= "S" else: fn &= "U"
      fn &= f.typ.size.intToStr
      if f.typ.size != 1:
        case f.endian
        of eLe: fn &= "Le"
        of eBe: fn &= "Be"
        of eNone:
          case e
          of eLe: fn &= "Le"
          of eBe: fn &= "Be"
          of eNone:
            echo "Could not determine endianness"
            quit QuitFailure
      letStmt = newLetStmt(
        ident(f.id),
        newCall(
          ident(fn),
          ident"io"))
    of ktkFloat:
      discard
    of ktkArray:
      discard
    of ktkStr:
      discard
    of ktkStrz:
      discard
    of ktkUser:
      letStmt = newLetStmt(
        ident(f.id),
        newCall(
          ident"read",
          ident(f.typ.id),
          ident"io",
          ident"root",
          ident"result"))
    result = newStmtList(
      letStmt,
      newAssignment(
        newDotExpr(
          ident"result",
          ident(f.id)),
        ident(f.id)))
  of true:
    let
      typ = f.typ.toNimType
      valDecl = newNimNode(nnkVarSection).add(
        newIdentDefs(
          ident(f.id & "Val"),
          nnkBracketExpr.newTree(
            ident"Option",
            typ)))
    letStmt = newLetStmt(
      ident(f.id),
      nnkLambda.newTree(
        newEmptyNode(),
        newEmptyNode(),
        newEmptyNode(),
        nnkFormalParams.newTree(typ),
        newEmptyNode(),
        newEmptyNode(),
        newStmtList(
          nnkDiscardStmt.newTree(
            newEmptyNode()))))
    result = newStmtList(
      valDecl,
      letStmt,
      newAssignment(
        newDotExpr(
          ident"result",
          ident(f.id & "Inst")),
        ident(f.id)))
  if f.size != nil:
    case f.typ.kind
    of ktkUser:
      result.add newCall(
        ident"skip",
        ident"io",
        infix(
          f.size.toNim,
          "-",
          newCall(
            ident"sizeof",
            ident(f.typ.id))))
    else: discard

proc typeDecl(t: Nimitype): seq[NimNode] =
  result = newSeq[NimNode](2)
  result[0] = nnkTypeDef.newTree(
    ident(t.id),
    newEmptyNode(),
    nnkRefTy.newTree(
      ident(t.id & "Obj")))

  result[1] = nnkTypeDef.newTree(
    ident(t.id & "Obj"),
    newEmptyNode())
  var
    obj = nnkObjectTy.newTree(
      newEmptyNode(),
      newEmptyNode())
    fields = newTree(nnkRecList)

  fields.add(
    nnkIdentDefs.newTree(
      ident"io",
      ident"KaitaiStream",
      newEmptyNode()),
    nnkIdentDefs.newTree(
      ident"root",
      ident(t.root),
      newEmptyNode()),
    nnkIdentDefs.newTree(
      ident"parent",
      parentType(t),
      newEmptyNode()))

  for f in t.fields:
    let typ = f.typ.toNimType
    fields.add(
      if f.isLazy:
        newIdentDefs(
          ident(f.id & "Inst"),
          nnkProcTy.newTree(
            nnkFormalParams.newTree(typ),
            newEmptyNode()))
      else:
        newIdentDefs(
          ident(f.id),
          typ))

  obj.add(fields)
  result[1].add(obj)

proc readProc(t: Nimitype): NimNode =
  let
    tIo = newIdentDefs(
      ident"io",
      ident"KaitaiStream")
    tRoot = newIdentDefs(
      ident"root",
      ident(t.root))
    tParent = newIdentDefs(
      ident"parent",
      parentType(t))
    tThis = ident(t.id)
    tDesc = newIdentDefs(
      ident"_",
      nnkBracketExpr.newTree(
        ident"typedesc",
        tThis))

  result = newProc(
    ident"read",
    @[tThis,
      tDesc,
      tIo,
      tRoot,
      tParent])
  result.body = newStmtList(
    newAssignment(
      ident"result",
      nnkObjConstr.newTree(
        tThis,
        newColonExpr(
          ident"io",
          ident"io"),
        newColonExpr(
          ident"parent",
          ident"parent"))),
    newLetStmt(
      ident"root",
      nnkIfExpr.newTree(
        nnkElifExpr.newTree(
          infix(
            ident"root",
            "==",
            newNilLit()),
          nnkCast.newTree(
            ident(t.root),
            ident"result")),
        nnkElseExpr.newTree(
          ident"root"))),
    newAssignment(
      newDotExpr(
        ident"result",
        ident"root"),
      ident"root"))

  for f in t.fields:
    result.body.add(readField(f, t.endian))

proc destroyProc(t: Nimitype): NimNode =
  let tObj = newIdentDefs(
    ident"x",
    nnkVarTy.newTree(
      ident(t.id & "Obj")))
  result = newProc(
    ident"destroy=",
    @[newEmptyNode(),
      tObj])
  result.body = newCall(
    ident"close",
    newDotExpr(
      ident"x",
      ident"io"))

proc fromFileProc(t: Nimitype): NimNode =
  let
    tThis = ident(t.id)
    tDesc = newIdentDefs(
      ident"td",
      nnkBracketExpr.newTree(
        ident"typedesc",
        tThis))
    tFilename = newIdentDefs(
      ident"filename",
      ident"string")

  result = newProc(
    ident"fromFile",
    @[tThis,
      tDesc,
      tFilename])
  result.body = newCall(
    ident"read",
    ident"td",
    newCall(
      ident"newKaitaiStream",
      ident"filename"),
    newNilLit(),
    newNilLit())

macro injectParser*(path: static[string]) =
  result = newStmtList()
  let types = parseKsyAst(path)
  var typeSection = newTree(nnkTypeSection)
  for t in types:
    typeSection.add(typeDecl(t))
  result.add typeSection

  # Template for pythonic @property behavior
  result.add nnkTemplateDef.newTree(
    nnkAccQuoted.newTree(
      ident"."),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      ident"untyped",
      newIdentDefs(
        ident"a",
        ident(types[^1].id)),
      newIdentDefs(
        ident"b",
        ident"untyped")),
    newEmptyNode(),
    newEmptyNode(),
    newStmtList(
      newCall(
        newPar(
          newDotExpr(
            ident"a",
            nnkAccQuoted.newTree(
              ident"b",
              ident"inst"))))))

  for t in types:
    result.add readProc(t)
    result.add destroyProc(t)
  result.add fromFileProc(types[^1])

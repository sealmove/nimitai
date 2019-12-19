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
      of 0: result = ident"uint"
      of 1: result = ident"uint8"
      of 2: result = ident"uint16"
      of 4: result = ident"uint32"
      of 8: result = ident"uint64"
      else: discard
    of true:
      case typ.size
      of 0: result = ident"int"
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
  of knkBool:
    result = newLit(expr.boolval)
  of knkInt:
    result = newLit(expr.intval)
  of knkFloat:
    result = newLit(expr.floatval)
  of knkStr:
    result = newLit(expr.strval)
  of knkArray:
    var b = newNimNode(nnkBracket)
    for i in expr.arrval:
      b.add(i.toNim)
    result = prefix(b, "@")
  of knkArithOp:
    let fn = ident($expr.ao)
    result = newCall(
      fn,
      expr.aoL.toNim,
      expr.aoR.toNim)
  of knkBitOp:
    let fn = ident($expr.bo)
    result = newCall(
      fn,
      expr.boL.toNim,
      expr.boR.toNim)
  of knkCmpOp:
    let fn = ident($expr.co)
    result = newCall(
      fn,
      expr.coL.toNim,
      expr.coR.toNim)
  of knkRelOp:
    let fn = ident($expr.ao)
    result = newCall(
      fn,
      expr.roL.toNim,
      expr.roR.toNim)
  of knkUnaryOp:
    let fn = ident($expr.uo)
    result = newCall(
      fn,
      expr.uoO.toNim)

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
  let typ = f.typ.toNimType

  if f.isLazy and f.value != nil: # Calculated value instance
    return newStmtList(
      newLetStmt(
        ident(f.id),
        newCall(
          typ,
          f.value.toNim)),
      newAssignment(
        newDotExpr(
          ident"result",
          ident(f.id)),
        ident(f.id)))

  var call: NimNode
  case f.typ.kind
  of ktkBit:
    call = newCall(
      typ,
      newCall(
        ident"readBitsInt",
        ident"io",
        newLit(f.typ.bits)))
  of ktkBool:
    call = newCall(
      ident"bool",
      newCall(
        ident"readBitsInt",
        ident"io",
        newLit(1)))
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
    call = newCall(
      ident(fn),
      ident"io")
  of ktkFloat:
    discard
  of ktkArray:
    discard
  of ktkStr:
    call = newCall(
      ident"read_string",
      ident"io",
      f.size.toNim)
  of ktkStrz:
    discard
  of ktkUser:
    call = newCall(
      ident"read",
      ident(f.typ.id),
      ident"io",
      ident"root",
      ident"result")

  case f.isLazy
  of false: # Attribute
    result = newStmtList(
      newLetStmt(
        ident(f.id),
        call),
      newAssignment(
        newDotExpr(
          ident"result",
          ident(f.id)),
        ident(f.id)))
  of true: # Instance
    let
      varDecl = newNimNode(nnkVarSection).add(
        newIdentDefs(
          ident(f.id & "Val"),
          nnkBracketExpr.newTree(
            ident"Option",
            typ)))
      valAssignment = newAssignment(
        ident(f.id & "Val"),
        newCall(
          ident"some",
          call))
      ifBody = if f.pos == nil:
                 newStmtList(valAssignment)
               else:
                 newStmtList(
                   newCall(
                     ident"skip",
                     ident"io",
                     f.pos.toNim),
                   valAssignment,
                   newCall(
                     ident"skip",
                     ident"io",
                     prefix(
                       f.pos.toNim,
                       "-")))
      body = newStmtList(
        newIfStmt(
          (newCall(
            ident"isNone",
            ident(f.id & "Val")),
           ifBody)),
        newCall(
          ident"get",
          ident(f.id & "Val")))
      letStmt = newLetStmt(
        ident(f.id),
        nnkLambda.newTree(
          newEmptyNode(),
          newEmptyNode(),
          newEmptyNode(),
          nnkFormalParams.newTree(typ),
          newEmptyNode(),
          newEmptyNode(),
          body))
      assignment = newAssignment(
        newDotExpr(
          ident"result",
          ident(f.id & "Inst")),
        ident(f.id))
    result = newStmtList(
      varDecl,
      letStmt,
      assignment)

  if f.typ.kind == ktkUser and f.size != nil:
    result.add newCall(
      ident"skip",
      ident"io",
      newCall(
        ident"aoSub",
        f.size.toNim,
        newCall(
          ident"sizeof",
          ident(f.typ.id))))

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
      if f.isLazy and f.value == nil:
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

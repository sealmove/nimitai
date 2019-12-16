import macros, strutils, nimitai/private/ast
#XXX Need to document necessary imports and pragmas
# imports: kaitai_struct_nim_runtime, options
# pragmas: dotOperators

proc toNimType*(typ: KsType): NimNode =
  case typ.kind
  of ktkNil:
    result = nnkBracketExpr.newTree(
               ident"seq",
               ident"byte")
  of ktkBit:
    case typ.bits
    of  1 ..  8: result = ident"uint8"
    of  9 .. 16: result = ident"uint16"
    of 17 .. 32: result = ident"uint32"
    of 33 .. 64: result = ident"uint64"
    else: discard
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
  of ktkBool:
    result = ident"bool"
  of ktkString:
    result = ident"string"
  of ktkEnum: #XXX
    discard
  of ktkUser:
    result = ident(typ.id)

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
  let name = ident(f.id)
  case f.typ
  of ktkNil:
  of ktkBit:
  of ktkInt:
  of ktkFloat:
  of ktkBool:
  of ktkString:
  of ktkEnum:
  of ktkUser:

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
    fields.add(
      nnkIdentDefs.newTree(
        ident(f.id),
        toNimType(f.typ),
        newEmptyNode()))

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
              idkstype
              ident"inst"))))))

  for t in types:
    result.add readProc(t)
    result.add destroyProc(t)
  result.add fromFileProc(types[^1])

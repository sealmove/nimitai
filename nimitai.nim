import macros, strutils, nimitai/private/ast, nimitai/private/kstype
#XXX Need to document necessary imports and pragmas
# imports: kaitai_struct_nim_runtime, options
# pragmas: dotOperators

proc toNimType(typ: KsType): NimNode =
  case typ
  of ktkNil:
    nnkBracketExpr.newTree(
      ident"seq",
      ident"byte")
  of ktkBit:
    case typ.bits
    of  1 ..  8: "uint8"
    of  9 .. 16: "uint16"
    of 17 .. 32: "uint32"
    of 33 .. 64: "uint64"
    else: discard
  of ktkInt:
    case typ.isSigned
    of false:
      case typ.size
      of 1: ident"uint8"
      of 2: ident"uint16"
      of 4: ident"uint32"
      of 8: ident"uint64"
      else: discard
    of true:
      of 1: ident"int8"
      of 2: ident"int16"
      of 4: ident"int32"
      of 8: ident"int64"
      else: discard
  of ktkFloat:
    case typ.precision:
    of 4: ident"float32"
    of 8: ident"float64"
  of ktkBool:
    ident"bool"
  of ktkString:
    ident"string"
  of ktkEnum: #XXX
    discard
  of ktkUser:
    ident(typ.id)

proc parentType(t: Nimitype): NimNode =
  if t.parent == "":
    nnkRefTy.newTree(
      ident"RootObj")
  else:
    ident(t.parent)

proc bits(typ: string): int =
  parseInt(typ[1..^1])

proc readField(f: Field, e: Endian): NimNode =
  let name = ident(f.id)
  if f.typ in @["u2", "u4", "u8", "s2", "s4", "s8"]:
    let fn = case e
             of eLe: "read" & f.typ & "le"
             of eBe: "read" & f.typ & "be"
             of eNone:
               echo "No endian specified"
               quit QuitFailure
    #XXX default endianess
    result = newStmtList(
      newLetStmt(
        name,
        newCall(
          fn,
          ident"io")),
      newAssignment(
        newDotExpr(
          ident"result",
          name),
        name))
  elif f.typ in @["u1", "s1", "u2le", "u2be", "u4le", "u4be", "u8le", "u8be",
                  "s2le", "s2be", "s4le", "s4be", "s8le", "s8be"]:
    result = newStmtList(
      newLetStmt(
        name,
        newCall(
          "read" & f.typ,
          ident"io")),
      newAssignment(
        newDotExpr(
          ident"result",
          name),
        name))
  elif isBitType(f.typ):
    let bits = bits(f.typ)
    result = newStmtList(
      newLetStmt(
        name,
        newCall(
          "readBitsInt",
          ident"io",
          newLit(bits))),
      newAssignment(
        newDotExpr(
          ident"result",
          name),
        name))
  else:
    result = newStmtList(
      newLetStmt(
        name,
        newCall(
          "read",
          ident(f.typ),
          ident"io",
          ident"root",
          ident"result")),
      newAssignment(
        newDotExpr(
          ident"result",
          name),
        name))
    if f.kind == fkArray and f.size != "":
      result.add(
        newCall(
          "skip",
          ident"io",
          infix(
            newCall(
              ident"int",
              parseKsExpr(f.size)),
            "-",
            newCall(
              ident"sizeof",
              name))))

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
  parseKsyAst(path)
  var typeSection = newTree(nnkTypeSection)
  for t in nimitypeTable:
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
        ident(nimitypeTable[^1].id)),
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

  for t in nimitypeTable:
    result.add readProc(t)
    result.add destroyProc(t)
  result.add fromFileProc(nimitypeTable[^1])

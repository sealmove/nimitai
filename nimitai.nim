import nimitai/private/ast, macros

proc ksToNim(ksType: string): NimNode =
  case ksType
  of "": nnkBracketExpr.newTree(
    ident"seq",
    ident"byte")
  of "u1": ident"uint8"
  of "u2", "u2le", "u2be": ident"uint16"
  of "u4", "u4le", "u4be": ident"uint32"
  of "u8", "u8le", "u8be": ident"uint64"
  of "s1": ident"int8"
  of "s2", "s2le", "s2be": ident"int16"
  of "s4", "s4le", "s4be": ident"int32"
  of "s8", "s8le", "s8be": ident"int64"
  else: ident(ksType)

proc parentType(t: Nimitype): NimNode =
  if t.parent == "":
    nnkRefTy.newTree(
      ident"RootObj")
  else:
    ident(t.parent)

proc readField(f: Field): NimNode =
  let
    call = case f.typ
    of "u2", "u4", "u8", "s2", "s4", "s8":
      #XXX default endianess
      newCall(
        "read" & f.typ & "be",
        ident"io")
    of "u1", "s1", "u2le", "u2be", "u4le", "u4be", "u8le", "u8be", "s2le",
       "s2be", "s4le", "s4be", "s8le", "s8be":
      newCall(
        "read" & f.typ,
        ident"io")
    else:
      newCall(
        "read",
        ident(f.typ),
        ident"io",
        ident"root",
        ident"result")
    name = ident(f.id)

  result = newStmtList(
    newLetStmt(
      name,
      call),
    newAssignment(
      newDotExpr(
        ident"result",
        name),
      name))

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
        ksToNim(f.typ),
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
    result.body.add(readField(f))

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

proc importSection(imports: varargs[string]): NimNode =
  result = newNimNode(nnkImportStmt)
  for i in imports:
    result.add(ident(i))

macro generateParser*(path: static[string]) =
  var types = parseKsyAst(path)
  result = newStmtList()
  #result.add importSection("nimitai/private/runtime")
  var typeSection = newTree(nnkTypeSection)
  for t in types:
    typeSection.add(typeDecl(t))
  result.add typeSection
  for t in types:
    result.add readProc(t)
    result.add destroyProc(t)
  result.add fromFileProc(types[^1])

import
  nimitai/private/[ksyast, ksypeg], macros, sequtils, strutils, strformat,
  tables

# Level 0 - String helpers
proc primative(ksType: string): string =
  case ksType
  of "u1": "uint8"
  of "u2", "u2le", "u2be": "uint16"
  of "u4", "u4le", "u4be": "uint32"
  of "u8", "u8le", "u8be": "uint64"
  of "s1": "int8"
  of "s2", "s2le", "s2be": "int16"
  of "s4", "s4le", "s4be": "int32"
  of "s8", "s8le", "s8be": "int64"
  else: ""

proc hierarchy(t: Type): seq[string] =
  var t = t
  while t.name != "RootObj":
    result.insert(t.name)
    t = t.parent

# Level 1 - Node helpers
proc parentType(t: Type): NimNode =
  if t.parent.name == "RootObj":
    nnkRefTy.newTree(ident"RootObj")
  else:
    ident(hierarchy(t.parent).join)

proc attrType(a: Attr, t: Type): NimNode =
  if kkType notin a.keys:
    return nnkBracketExpr.newTree(
      newIdentNode("seq"),
      newIdentNode("byte"))

  let prim = primative(a.keys[kkType].strval)
  if prim != "": return ident(prim)

  var t = t
  let typ = a.keys[kkType].strval.capitalizeAscii
  while typ notin t.types.mapIt(it.name):
    t = t.parent
  return ident(hierarchy(t).join & typ)

# Level 2 - Generators
proc genTypes(stmts: var NimNode, t: Type) =
  #XXX: doc

  for typ in t.types:
    genTypes(stmts, typ)

  let
    name = hierarchy(t).join
    objName = name & "Obj"

  var res = newSeq[NimNode](2)
  res[0] = nnkTypeDef.newTree(
    ident(name),
    newEmptyNode(),
    nnkRefTy.newTree(ident(objName)))
  res[1] = nnkTypeDef.newTree(
    ident(objName),
    newEmptyNode())

  var
    obj = nnkObjectTy.newTree(
      newEmptyNode(),
      newEmptyNode())
    fields = newTree(nnkRecList)

  fields.add(nnkIdentDefs.newTree(
    ident"io",
    ident"KaitaiStream",
    newEmptyNode()),
  nnkIdentDefs.newTree(
    ident"root",
    ident(t.root.name),
    newEmptyNode()),
  nnkIdentDefs.newTree(
    ident"parent",
    parentType(t),
    newEmptyNode()))

  for a in t.attrs:
    fields.add(nnkIdentDefs.newTree(
      ident(a.id),
      attrType(a, t),
      newEmptyNode()))

  obj.add(fields)
  res[1].add(obj)
  stmts.add(res)

proc genReadAndDestroy(stmts: var NimNode, t: Type) =
  for typ in t.types:
    genReadAndDestroy(stmts, typ)

  let
    tIo = newIdentDefs(
      ident"io",
      ident"KaitaiStream")
    tRoot = newIdentDefs(
      ident"root",
      ident(t.root.name))
    tParent = newIdentDefs(
      ident"parent",
      parentType(t))
    tThis = ident(hierarchy(t).join)
    tObj = newIdentDefs(
      ident"x",
      nnkVarTy.newTree(ident(hierarchy(t).join & "Obj")))
    tDesc = newIdentDefs(
      ident"_",
      nnkBracketExpr.newTree(
        ident"typedesc",
        tThis))

  #XXX: for attr in t.attrs:

  # Read
  var read = newProc(
    ident"read",
    @[tThis,
      tDesc,
      tIo,
      tRoot,
      tParent])
  read.body = newStmtList(
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
          ident"result"),
        nnkElseExpr.newTree(
          ident"root"))),
    newAssignment(
      newDotExpr(
        ident"result",
        ident"root"),
      ident"root"))

  # Destroy
  var destroy = newProc(
    ident"destroy=",
    @[newEmptyNode(),
      tObj])
  destroy.body = newCall(
    ident"close",
    newDotExpr(
      ident"x",
      ident"io"))

  stmts.add(read, destroy)

# Level 3 - codegen ordering
proc imp(i: string): NimNode =
  newNimNode(nnkImportStmt).add(ident(i))

proc types(t: Type): NimNode =
  result = newTree(nnkTypeSection)
  result.genTypes(t)

proc readAndDestroy(t: Type): NimNode =
  result = newStmtList()
  result.genReadAndDestroy(t)

proc fromFile(t: Type): NimNode =
  let
    tThis = ident(hierarchy(t).join)
    tDesc = newIdentDefs(
      ident"_",
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
    tThis,
    newCall(
      ident"newKaitaiStream",
      ident"filename"),
    newNilLit(),
    newNilLit())

macro generateParser*(path: static[string]) =
  var maintype = parseKsy(path)
  result = newStmtList(
    imp("nimitai/private/runtime"),
    types(maintype),
    readAndDestroy(maintype),
    fromFile(maintype))

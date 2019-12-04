import
  nimitai/private/[ksyast, ksypeg], macros, sequtils, strutils, strformat,
  tables

const primitiveTypes = {
  "u1"   : "uint8"  ,
  "u2le" : "uint16" ,
  "u2be" : "uint16" ,
  "u4le" : "uint32" ,
  "u4be" : "uint32" ,
  "u8le" : "uint64" ,
  "u8be" : "uint64" ,
  "s1"   : "int8"   ,
  "s2le" : "int16"  ,
  "s2be" : "int16"  ,
  "s4le" : "int32"  ,
  "s4be" : "int32"  ,
  "s8le" : "int64"  ,
  "s8be" : "int64"
}.toTable

# Level 0 - Helper procedures
proc hierarchy(t: Type): seq[string] =
  var t = t
  while t.name != "RootObj":
    result.insert(t.name)
    t = t.parent

proc parentType(t: Type): NimNode =
  if t.parent.name == "RootObj":
    nnkRefTy.newTree(ident"RootObj")
  else:
    ident(hierarchy(t.parent).join)

proc resolveAttrType(a: Attr, t: Type): string =
  if kkType notin a.keys:
    ksyError(&"Attribute {a.id} has no type" &
             "This should work after implementing typeless attributes" &
             "https://doc.kaitai.io/ksy_reference.html#attribute-type")
  if a.keys[kkType].strval in primitiveTypes:
    return primitiveTypes[a.keys[kkType].strval]

  let ksType = a.keys[kkType].strval.capitalizeAscii
  var t = t

  while true:
    if ksType in t.types.mapIt(it.name):
      return hierarchy(t).join & ksType
    t = t.parent

# Level 1 - Generators
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
    obj = nnkObjectTy.newTree(newEmptyNode(), newEmptyNode())
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
      ident(resolveAttrType(a, t)),
      newEmptyNode()))

  obj.add(fields)
  res[1].add(obj)
  stmts.add(res)

proc genReadAndDestroy(stmts: var NimNode, t: Type) =
  for typ in t.types:
    genReadAndDestroy(stmts, typ)

  # Read
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

  var read = newProc(ident"read", @[tThis, tDesc, tIo, tRoot, tParent])
  read.body = nnkAsgn.newTree(
    ident"result",
    nnkObjConstr.newTree(
      tThis,
      newColonExpr(
        ident"io",
        ident"io"),
      newColonExpr(
        ident"root",
        ident"root"),
      newColonExpr(
        ident"parent",
        ident"parent")))

  # Destroy
  var destroy = newProc(ident"destroy=", @[newEmptyNode(), tObj])
  destroy.body = newCall(
    ident"close",
    newDotExpr(
      ident"x",
      ident"io"))

  stmts.add(read, destroy)

# Level 3
proc imp(i: string): NimNode =
  newNimNode(nnkImportStmt).add(ident(i))

proc types(t: Type): NimNode =
  result = newTree(nnkTypeSection)
  result.genTypes(t)

proc readAndDestroy(t: Type): NimNode =
  result = newStmtList()
  result.genReadAndDestroy(t)

macro generateParser*(path: static[string]) =
  var maintype = parseKsy(path)
  result = newStmtList(
    imp("nimitai/private/runtime"),
    types(maintype),
    readAndDestroy(maintype))

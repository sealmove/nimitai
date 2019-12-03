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

proc newImport(i: string): NimNode =
  newNimNode(nnkImportStmt).add(newIdentNode(i))

proc resolveType(a: Attr, tnode: Type, h: seq[string]): string =
  if kkType notin a.keys:
    ksyError(&"Attribute {a.id} has no type" &
             "This should work after implementing typeless attributes" &
             "https://doc.kaitai.io/ksy_reference.html#attribute-type")
  if a.keys[kkType].strval in primitiveTypes:
    return primitiveTypes[a.keys[kkType].strval]
  let ksType = a.keys[kkType].strval.capitalizeAscii
  var
    h = h
    tnode = tnode
  while true:
    if ksType in tnode.types.mapIt(it.name):
      return h.join & tnode.name & ksType
    if ksType in tnode.parent.types.mapIt(it.name):
      return h.join & ksType
    discard h.pop
    tnode = tnode.parent

proc genType(ts: var NimNode, t: Type, h: seq[string] = @[]) =
  #XXX: doc
  let name = h.join & t.name

  for typ in t.types:
    genType(ts, typ, h & t.name)

  let objName = name & "Obj"

  var res = newSeq[NimNode](2)
  res[0] = nnkTypeDef.newTree(
    ident(name),
    newEmptyNode(),
    nnkRefTy.newTree(ident(objName)))
  res[1] = nnkTypeDef.newTree(ident(objName), newEmptyNode())

  var
    obj = nnkObjectTy.newTree(newEmptyNode(), newEmptyNode())
    fields = newTree(nnkRecList)


  let parentType = if t.parent.name == "RootObj":
                     nnkRefTy.newTree(ident"RootObj")
                   else:
                     ident(h.join)
  fields.add(
    nnkIdentDefs.newTree(
      ident"io",
      ident"KaitaiStream",
      newEmptyNode()
    ),
    nnkIdentDefs.newTree(
      ident"root",
      ident(t.root.name),
      newEmptyNode()
    ),
    nnkIdentDefs.newTree(
      ident"parent",
      parentType,
      newEmptyNode()
    )
  )

  for a in t.attrs:

    fields.add(
      nnkIdentDefs.newTree(
        ident(a.id),
        ident(a.resolveType(t, h)),
        newEmptyNode()
      )
    )

  obj.add(fields)
  res[1].add(obj)
  ts.add(res)

proc genRead(t: Type): NimNode =
  let
    parentNode = if t.parent.name == "RootObj":
                   nnkRefTy.newTree(ident"RootObj")
                 else:
                   ident(t.parent.name)
    typ = ident(t.name)
    desc = newIdentDefs(ident"_", nnkBracketExpr.newTree(ident"typedesc", typ))
    io = newIdentDefs(ident"io", ident"KaitaiStream")
    root = newIdentDefs(ident"root", ident(t.root.name))
    parent = newIdentDefs(ident"parent", parentNode)
  var body = newTree(nnkStmtList)
  body.add(
    nnkAsgn.newTree(
      ident"result",
      nnkObjConstr.newTree(
        typ,
        newColonExpr(ident"io", ident"io"),
        newColonExpr(ident"root", ident"root"),
        newColonExpr(ident"parent", ident"parent")
      )
    )
  )
  #for attr in t.attrs:

  result = newProc(ident"read", @[typ, desc, io, root, parent])
  result.body = body

macro generateParser*(path: static[string]) =
  let maintype = parseKsy(path)
  result = newStmtList()

  # Import statement
  result.add newImport("nimitai/private/runtime")

  # Type section
  var typesect = newTree(nnkTypeSection)
  typesect.genType(maintype)
  result.add(typesect)

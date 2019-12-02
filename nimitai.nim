import nimitai/private/[ksyast, ksypeg], macros, strutils, strformat, tables

proc newImport(i: string): NimNode =
  newNimNode(nnkImportStmt).add(newIdentNode(i))

proc attrType(a: Attr): string =
  if kkType notin a.keys:
    ksyError(&"Attribute {a.id} has no type" &
             "This should work after implementing typeless attributes" &
             "https://doc.kaitai.io/ksy_reference.html#attribute-type")
  let ksType = a.keys[kkType].strval
  result = case ksType
           of "u4": "uint32"
           of "u1": "uint8"
           of "str":"string"
           of "s4":"int32"
           else: ksType.capitalizeAscii

proc genType(t: Type): seq[NimNode] =
  #XXX: doc
  let
    objName = t.name & "Obj"

  result = newSeq[NimNode](2)
  result[0] = nnkTypeDef.newTree(
    ident(t.name),
    newEmptyNode(),
    nnkRefTy.newTree(ident(objName)))
  result[1] = nnkTypeDef.newTree(ident(objName), newEmptyNode())

  var
    obj = nnkObjectTy.newTree(newEmptyNode(), newEmptyNode())
    fields = newTree(nnkRecList)

  fields.add(
    nnkIdentDefs.newTree(
      ident"io",
      ident"KaitaiStream",
      newEmptyNode()
    ),
    nnkIdentDefs.newTree(
      ident"root",
      ident(t.root),
      newEmptyNode()
    ),
    nnkIdentDefs.newTree(
      ident"parent",
      nnkRefTy.newTree(ident(t.parent)),
      newEmptyNode()
    )
  )
  
  for a in t.attrs:
    fields.add(
      nnkIdentDefs.newTree(
        ident(a.id),
        ident(attrType(a)),
        newEmptyNode()
      )
    )

  obj.add(fields)
  result[1].add(obj)

proc genRead(t: Type): NimNode =
  let
    parentNode = if t.parent == "RootObj":
                   nnkRefTy.newTree(ident(t.parent))
                 else:
                   ident(t.parent)
    typ = ident(t.name)
    desc = newIdentDefs(ident"_", nnkBracketExpr.newTree(ident"typedesc", typ))
    io = newIdentDefs(ident"io", ident"KaitaiStream")
    root = newIdentDefs(ident"root", ident(t.root))
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
  let ksy = parseKsy(path)
  result = newStmtList()

  # Import statement
  result.add newImport("nimitai/private/runtime")

  # Type section
  var typesect = newTree(nnkTypeSection)
  for t in ksy.types:
    typesect.add genType(t)
  result.add(typesect)
  for t in ksy.types:
    result.add(genRead(t))

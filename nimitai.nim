import nimitai/private/[ksyast, ksypeg], macros, strutils, strformat

proc newImport(i: string): NimNode =
  newNimNode(nnkImportStmt).add(newIdentNode(i))

proc attrType(a: Attr): string =
  for k in a.keys:
    if k.kind == kkType:
      result = case k.strval
        of "u4": "uint32"
        of "u1": "uint8"
        of "str":"string"
        of "s4":"int32"
        else: k.strval.capitalizeAscii
    else:
      echo &"Attribute {a.id} has no type"
      echo "This should work after implementing typeless attributes"
      echo "https://doc.kaitai.io/ksy_reference.html#attribute-type"
      quit QuitFailure

proc newType(t: Type): seq[NimNode] =
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
      ident("io"),
      ident("KaitaiStream"),
      newEmptyNode()
    ),
    nnkIdentDefs.newTree(
      ident("root"),
      ident(t.root),
      newEmptyNode()
    ),
    nnkIdentDefs.newTree(
      ident("parent"),
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
  result[1].add((obj))

  return 

macro generateParser*(path: static[string]) =
  let ksy = parseKsy(path)
  result = newStmtList()

  # Import statement
  result.add newImport("nimitai/private/runtime")

  # Type section
  var typesect = newTree(nnkTypeSection)
  for t in ksy.types:
    typesect.add newType(t)
  result.add(typesect)

import nimitai/private/[ksyast, ksypeg], macros, strutils

proc newImport(i: string): NimNode =
  newNimNode(nnkImportStmt).add(newIdentNode(i))

proc newType(t: Type): seq[NimNode] =
  #XXX: doc
  let
    objName = t.name & "Obj"

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
    ),
    nnkIdentDefs.newTree(
      ident("hello"),
      ident("uint8"),
      newEmptyNode()
    )
  )
  
  #XXX: attrs
  #for a in t.attrs:
  
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

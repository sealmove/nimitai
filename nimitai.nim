import nimitai/private/[ksyast, ksypeg], macros, strutils

macro generateParser*(path: static[string]) =
  let ksy = parseKsy(path)
  result = newStmtList()

  # Import statement
  result.add newNimNode(nnkImportStmt).add(
    newIdentNode("nimitai/private/runtime"))

  # Type section
  var typesect = newTree(nnkTypeSection)
  for t in ksy.types:
    let
      name = t.name.capitalizeAscii
      objName = name & "Obj"
      refDef = nnkTypeDef.newTree(
        ident(name),
        newEmptyNode(),
        nnkRefTy.newTree(ident(objName))
      )
    typesect.add(refDef)
    var
      objDef = nnkTypeDef.newTree(ident(objName), newEmptyNode())
      objVal = nnkObjectTy.newTree(newEmptyNode(), newEmptyNode())
      fields = newTree(nnkRecList)
    fields.add(
      nnkIdentDefs.newTree(
        nnkPostfix.newTree(
          newIdentNode("*"),
          newIdentNode("io")
        ),
        newIdentNode("KaitaiStream"),
        newEmptyNode()
      ),
      nnkIdentDefs.newTree(
        nnkPostfix.newTree(
          newIdentNode("*"),
          newIdentNode("root")
        ),
        newIdentNode(name),
        newEmptyNode()
      ),
      nnkIdentDefs.newTree(
        nnkPostfix.newTree(
          newIdentNode("*"),
          newIdentNode("parent")
        ),
        nnkRefTy.newTree(
          newIdentNode("RootObj")
        ),
        newEmptyNode()
      ),
      nnkIdentDefs.newTree(
        nnkPostfix.newTree(
          newIdentNode("*"),
          newIdentNode("hello")
        ),
        newIdentNode("uint8"),
        newEmptyNode()
      )
    )
    #for a in t.attrs:
    objVal.add(fields)
    objDef.add(objVal)
    typesect.add(objDef)
  result.add(typesect)

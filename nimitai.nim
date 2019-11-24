import nimitai/private/[ksyast, ksypeg], macros

macro generateParser*(path: static[string]) =
  let ksy = parseKsy(path)
  result = newStmtList()

  # Generate import statement
  result.add newNimNode(nnkImportStmt).add(newIdentNode("nimitai"))

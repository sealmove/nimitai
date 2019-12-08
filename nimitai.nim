import nimitai/private/ast, macros

proc root(types: seq[Nimitype]): Nimitype = types[^1]

proc imp(i: string): NimNode =
  newNimNode(nnkImportStmt).add(ident(i))

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

proc typeSect(types: seq[Nimitype]): NimNode =
  result = newTree(nnkTypeSection)
  for t in types:
    var res = newSeq[NimNode](2)

    res[0] = nnkTypeDef.newTree(
      ident(t.id),
      newEmptyNode(),
      nnkRefTy.newTree(
        ident(t.id & "Obj")))
    res[1] = nnkTypeDef.newTree(
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
        if t.parent == "":
          nnkRefTy.newTree(
            ident"RootObj")
        else: ident(t.parent),
        newEmptyNode()))

    for f in t.fields:
      fields.add(
        nnkIdentDefs.newTree(
          ident(f.id),
          ksToNim(f.typ),
          newEmptyNode()))

    obj.add(fields)
    res[1].add(obj)
    result.add(res)

macro generateParser*(path: static[string]) =
  var types = parseKsyAst(path)
  result = newStmtList(
    imp("nimitai/private/runtime"),
    typeSect(types))

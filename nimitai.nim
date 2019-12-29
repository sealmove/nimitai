import macros, tables
import nimitai/[parser, exprlang]

var roott {.compileTime.}: NimNode

proc nim(e: Expr): NimNode =
  case e.kind
  of ekId:      result = ident(e.strVal)
  of ekInteger: result = newLit(e.intVal)
  of ekFloat:   result = newLit(e.floatVal)
  of ekBoolean: result = newLit(e.boolVal)
  of ekArray:   result = newLit[byte](e.arrayVal)
  of ekString:  result = newLit(e.strVal)
  of ekInfix:   result = infix(nim(e.left), e.infix, nim(e.right))
  of ekPrefix:  result = prefix(nim(e.operant), e.prefix)

proc typeDecl(t: Type, parentt: NimNode): seq[NimNode] =
  result = newSeq[NimNode](2)
  result[0] = nnkTypeDef.newTree(
    ident(t.name),
    newEmptyNode(),
    nnkRefTy.newTree(
      ident(t.name & "Obj")))

  result[1] = nnkTypeDef.newTree(
    ident(t.name & "Obj"),
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
      roott,
      newEmptyNode()),
    nnkIdentDefs.newTree(
      ident"parent",
      parentt,
      newEmptyNode()))

  obj.add(fields)
  result[1].add(obj)

proc addTypeDecl(ts: var NimNode, ct: Type, p: NimNode) =
  if skTypes in ct.sects:
    let types = ct.sects[skTypes].types
    for t in types.keys:
      ts.addTypeDecl((t, types[t]), ident(ct.name))
  ts.add typeDecl(ct, p)

macro injectParser*(path: static[string]) =
  result = newStmtList()
  var typeSection = newTree(nnkTypeSection)
  let
    mt = parse(path)
    id = mt[skMeta].meta[kkId].item
    parentt = nnkRefTy.newTree(ident"RootObj")
  roott = ident(id)
  typeSection.add typeDecl((id, mt), parentt)
  for t in mt[skTypes].types.pairs:
    typeSection.addTypeDecl(t, ident(id))
  result.add typeSection

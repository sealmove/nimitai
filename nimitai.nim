import macros, tables, strutils
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

proc addTypeDecl(ts: var NimNode, h: seq[string], sects: Sects) =
  ts.add typeDecl((h.join, sects), ident(h[0..^2].join))
  if skTypes in sects:
    let types = sects[skTypes].types
    for (t, s) in types.pairs:
      ts.addTypeDecl(h & t.capitalizeAscii, s)

macro injectParser*(path: static[string]) =
  result = newStmtList()
  var typeSection = newTree(nnkTypeSection)
  let
    mt = parse(path)
    id = mt[skMeta].meta[kkId].item.capitalizeAscii
  roott = ident(id)
  typeSection.add typeDecl((id, mt), nnkRefTy.newTree(ident"RootObj"))
  for (t, s) in mt[skTypes].types.pairs:
    typeSection.addTypeDecl(@[id, t.capitalizeAscii], s)
  result.add typeSection

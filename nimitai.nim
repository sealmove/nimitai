import macros, tables, strutils, sequtils, parseutils
import nimitai/[parser, exprlang]

var
  rootType {.compileTime.}: NimNode
  mt {.compileTime.}: Type

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

proc hierarchy(t: Type): seq[string] =
  var t = t
  while t != nil:
    result.insert(t.name.capitalizeAscii)
    t = t.parent

proc parentType(t: Type): NimNode =
  if t.parent == nil:
    nnkRefTy.newTree(ident"RootObj")
  else:
    ident(hierarchy(t.parent).join)

proc attributes(t: Type): seq[NimNode] =
  for attr in t.sects[skSeq].`seq`:
    # Translate type
    var typ: NimNode
    if kkType notin attr:
      typ = nnkBracketExpr.newTree(
              ident"seq",
              ident"byte")
    else:
      let ksType = attr[kkType].item
      case ksType
      of "u1":
        typ = ident"uint8"
      of "u2", "u2le", "u2be":
        typ = ident"uint16"
      of "u4", "u4le", "u4be":
        typ = ident"uint32"
      of "u8", "u8le", "u8be":
        typ = ident"uint64"
      of "s1":
        typ = ident"int8"
      of "s2", "s2le", "s2be":
        typ = ident"int16"
      of "s4", "s4le", "s4be":
        typ = ident"int32"
      of "s8", "s8le", "s8be":
        typ = ident"int64"
      of "f4", "f4le", "f4be":
        typ = ident"float32"
      of "f8", "f8le", "f8be":
        typ = ident"float64"
      of "str", "strz":
        typ = ident"string"
      of "b1":
        typ = ident"bool"
      else:
        var bits: int
        let parsedChars = ksType[1..^1].parseInt(bits)
        if ksType.startsWith("b") and parsedChars == ksType.len - 1:
          case bits
          of  1 ..  8: typ = ident"uint8"
          of  9 .. 16: typ = ident"uint16"
          of 17 .. 32: typ = ident"uint32"
          of 33 .. 64: typ = ident"uint64"
          else:        typ = nnkBracketExpr.newTree(
                                ident"seq",
                                ident"byte")
        else:
          # User type
          var t = t
          while skTypes in t.sects and
                ksType notin t.sects[skTypes].types.mapIt(it.name):
            t = t.parent
          typ = ident(hierarchy(t).join & ksType.capitalizeAscii)

    result.add(
      nnkIdentDefs.newTree(
        ident(attr[kkId].item),
        typ,
        newEmptyNode()))

proc typeDecl(t: Type): seq[NimNode] =
  result = newSeq[NimNode](2)

  let name = hierarchy(t).join

  result[0] = nnkTypeDef.newTree(
    ident(name),
    newEmptyNode(),
    nnkRefTy.newTree(
      ident(name & "Obj")))

  result[1] = nnkTypeDef.newTree(
    ident(name & "Obj"),
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
      rootType,
      newEmptyNode()),
    nnkIdentDefs.newTree(
      ident"parent",
      t.parentType,
      newEmptyNode()))

  fields.add t.attributes

  obj.add(fields)
  result[1].add(obj)

proc addTypeDecl(ts: var NimNode, t: Type) =
  if skTypes in t.sects:
    for typ in t.sects[skTypes].types:
      ts.addTypeDecl(typ)
  ts.add typeDecl(t)

macro injectParser*(path: static[string]) =
  result = newStmtList()
  var typeSection = newTree(nnkTypeSection)
  mt = parse(path)
  rootType = ident(mt.name.capitalizeAscii)
  if skTypes in mt.sects:
    for t in mt.sects[skTypes].types:
      typeSection.addTypeDecl(t)
  typeSection.add typeDecl(mt)
  result.add typeSection

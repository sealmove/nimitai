import npeg, ksyast, strutils, strformat

#[XXX
  doc-ref Sect
  KsyExpression <-
  error handling for enums
]#

type Ksy = ref object
  maintype*: Type
  types*: seq[Type]
  insts*: seq[Inst]
  enums*: seq[Enum]

var
  maintype = Type()
  types: seq[Type]
  insts: seq[Inst]
  enums: seq[Enum]
  sects: seq[Sect]
  attrs: seq[Attr]
  keys:  seq[Key]
  elems: seq[string]
  pairs: seq[tuple[key: int, value: string]]

proc push(stack: var seq[Key], kind: KeyKind, s: string = "",
          blist: seq[byte] = @[], slist: seq[string] = @[]) =
  var k = Key(kind: kind)
  case kind
  of kkApp, kkEncoding, kkId, kkLicense, kkTitle, kkType:
    k.strval = s
  of kkConsume:
    k.consume = parseBool(s)
  of kkContents:
    k.contents.add blist
  of kkEndian:
    k.endian = if s == "le": le else: be
  of kkExts, kkImports:
    k.list.add slist
  of kkRepeat:
    k.repeat = if s == "expr": expr elif s == "eos": eos else: until
  of kkSize:
    discard
  stack.add k

proc parseKsy*(path: string): Ksy =
  let p = peg "ksy":
    # Templates
    K(item) <- item * Colon
    ArrayInline(item) <- '[' * B * item * B * *(',' * B * item * B) * ']'
    Array2(item) <- +(+'\n' * ' '[2] * item * B)
    Array4(item) <- +(+'\n' * ' '[4] * item * B)
    Array6(item) <- +(+'\n' * ' '[6] * item * B)
    Array8(item) <- +(+'\n' * ' '[8] * item * B)
    YamlArray2(item) <- +(+'\n' * ' '[2] * Tag * item * B)
    YamlArray4(item) <- +(+'\n' * ' '[4] * Tag * item * B)
    YamlArray6(item) <- +(+'\n' * ' '[6] * Tag * item * B)

    # Atoms
    B <- *Blank
    Any <- +(1 - '\n')
    Tag <- '-' * B
    Colon <- B * ':' * B
    Bool <- "true" | "false"
    String <- '"' * *(1 - {'"', '\n'}) * '"'
    Identifier <- {'a'..'z'} * *{'a'..'z','0'..'9','_'}
    FileName <- >+{'A'..'Z','a'..'z','0'..'9','_','-','/'}:
      elems.add $1
    ArrayElem <- >(String | "0x" * +Xdigit | +Digit):
      elems.add $1

    # Main grammar
    ksy <- +(Sect * +'\n') * !1:
      types.add maintype
    Sect <- Meta | Doc | Seq | Types | Insts | Enums
    Sect4 <- ' '[4] * (Meta4 | Doc4 | Seq4)
    Types <- K("types") * Array2(TypesType)
    Meta <- K("meta") * Array2(Key):
      if maintype.meta != @[]:
        echo "Meta section is defined more than once"
        quit QuitFailure
      var
        sect = Sect(kind: skMeta)
        isId: bool
      while keys.len > 0:
        sect.keys.add(keys.pop)
      for k in sect.keys:
        if k.kind == kkId:
          maintype.name = k.strval
          isId = true
          break
      if isId:
        maintype.meta = sect.keys
      else:
        echo "Missing id in meta section"
        quit QuitFailure
    Doc <- K("doc") * >(('|' * B * *(+'\n' * ' '[2] * Any)) | Any):
      maintype.doc = $1
    Seq <- K("seq") * YamlArray2(Attr):
      var sect = Sect(kind: skSeq)
      while attrs.len > 0:
        sect.attrs.add(attrs.pop)
      maintype.attrs = sect.attrs
    Meta4 <- K("meta") * Array6(Key):
      var sect = Sect(kind: skMeta)
      while keys.len > 0:
        sect.keys.add(keys.pop)
      sects.add sect
    Doc4 <- K("doc") * >(('|' * B * *(+'\n' * ' '[6] * Any)) | Any):
      sects.add Sect(kind: skDoc, doc: $1)
    Seq4 <- K("seq") * YamlArray6(Attr):
      var sect = Sect(kind: skSeq)
      while attrs.len > 0:
        sect.attrs.add(attrs.pop)
      sects.add sect
    Insts <- K("instances") * Array2(Inst)
    Enums <- K("enums") * Array2(Enum)
    Key <- App | Consume | Contents | Encoding | Endian | EnumKey | EosError |
           Exts | Id | If | Include | Imports | Io | License | Process | Pos |
           Repeat | RepeatExpr | RepeatUntil | Size | SizeEos | Terminator |
           Title | Type | Value
    Attr <- K("id") * >Identifier * (Array4(Key) | Array8(Key)):
      var attr = Attr(id: $1)
      while keys.len > 0:
        attr.keys.add(keys.pop)
      attrs.add attr
    TypesType <- K(>Identifier) * +(+'\n' * Sect4):
      var
        t = Type(name: $1)
        flags: set[SectKind]
        s = $1
      while sects.len > 0:
        let n = sects.pop
        if flags.contains(n.kind):
          echo &"{n.kind} field for {s} declared more than once"
          quit QuitFailure
        flags.incl(n.kind)
        case n.kind
        of skMeta:
          t.meta = n.keys
        of skDoc:
          t.doc = n.doc
        of skSeq:
          t.attrs = n.attrs
        else:
          discard # If this is reached, something went wrong
      types.add t
    Inst <- K(>Identifier) * Array4(Key):
      var i = Inst(name: $1)
      while keys.len > 0:
        i.keys.add(keys.pop)
      insts.add i
    Enum <- K(>Identifier) * Array4(Pair):
      let e = Enum(name: $1)
      while pairs.len > 0:
        e.pairs.add(pairs.pop)
      enums.add e
    Pair <- K(>+Digit) * >Identifier:
      pairs.add (parseInt($1), $2)

    # Keys
    App <- K("application") * >Any:
      keys.push(kkApp, $1)
    Consume <- K("consume") * >Bool * B:
      keys.push(kkConsume, $1)
    Contents <- K("contents") *
                (ArrayElem | ArrayInline(ArrayElem) | YamlArray6(ArrayElem)):
      var list: seq[byte]
      while elems.len > 0:
        list.stackBytes(elems.pop)
      keys.push(kkContents, blist = list)
    Encoding <- K("encoding") * >Any:
      keys.push(kkEncoding, $1)
    Endian <- K("endian") * >("le" | "be"):
      keys.push(kkEndian, $1)
    Exts <- K("file-extension") * (YamlArray4(FileName) | FileName):
      var list: seq[string]
      while elems.len > 0:
        list.insert(elems.pop, 0)
      keys.push(kkExts, slist = list)
    Id <- K("id") * >Identifier:
      keys.push(kkId, $1)
    Imports <- K("imports") * (YamlArray4(FileName) | FileName):
      var list: seq[string]
      while elems.len > 0:
        list.insert(elems.pop, 0)
      keys.push(kkImports, slist = list)
    License <- K("license") * >Any:
      keys.push(kkLicense, $1)
    Repeat <- K("repeat") * >("expr" | "eos" | "until"):
      keys.push(kkRepeat, $1)
    Size <- K("size") * >Any:
      keys.push(kkRepeat, $1)
    Title <- K("title") * >Any:
      keys.push(kkTitle, $1)
    Type <- K("type") * >Identifier:
      keys.push(kkType, $1)

    #XXX
    EnumKey <- K("enum") * Any
    EosError <- K("eos-error") * Any
    If <- K("if") * Any # Expression
    Include <- K("include") * Any
    Io <- K("io") * Any
    Process <- K("process") * Any
    Pos <- K("pos") * Any
    RepeatExpr <- K("repeat-expr") * Any # Expression
    RepeatUntil <- K("repeat-until") * Any # Expression
    SizeEos <- K("size-eos") * Any
    Terminator <- K("terminator") * Any
    Value <- K("value") * Any # Expression

  doAssert p.matchFile(path).ok

  Ksy(maintype: maintype, types: types, insts: insts, enums: enums)

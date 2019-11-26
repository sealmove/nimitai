import npeg, ksyast, strutils, strformat

#[XXX
  doc-ref Sect
  KsyExpression <-
  error handling for enums
]#

var
  maintype {.compileTime.} = Type(parent: "RootObj")
  types {.compileTime.}: seq[Type]
  insts {.compileTime.}: seq[Inst]
  enums {.compileTime.}: seq[Enum]
  sects {.compileTime.}: seq[Sect]
  attrs {.compileTime.}: seq[Attr]
  keys  {.compileTime.}: seq[Key]
  elems {.compileTime.}: seq[string]
  pairs {.compileTime.}: seq[tuple[key: int, value: string]]
  level {.compileTime.}: int
  inds {.compileTime.}: seq[int]

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
    Array(item) <- +(C * item * B * +'\n')
    YamlArray(item) <- +(C * Tag * item * B * +'\n')

    # Indentation control
    C <- >*' ':
      doAssert len($1) == level
    I <- +'\n' * >*&' ':
      let indent = len($1) - level
      doAssert indent > 0
      inds.add(indent)
      level = len($1)
    D <- &1:
      dec(level, inds.pop)
    AttrTag <- '-' * >*' ':
      let indent = 1 + len($1)
      inds.add(indent)
      inc(level, indent)

    # Atoms
    B <- *Blank
    Line <- +(1 - '\n')
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
    ksy <- +Sect * !1:
      types.add maintype
    Sect <- (Meta | Doc | Seq | Types | Insts | Enums) * D
    Types <- K("types") * Array(Type) * D
    Meta <- K("meta") * I * Array(Key):
      var sect = Sect(kind: skMeta)
      while keys.len > 0:
        sect.keys.add(keys.pop)
      if level == 0:
        if maintype.meta != @[]:
          echo "Meta section is defined more than once"
          quit QuitFailure
        var isId: bool
        for k in sect.keys:
          if k.kind == kkId:
            maintype.name = k.strval
            isId = true
            maintype.meta = sect.keys
            break
        if not isId:
          echo "Missing id in meta section"
          quit QuitFailure
      else:
        sects.add sect
    Doc <- K("doc") * >(('|' * B * I * +(C * Line) * D) | Line):
      if level == 0:
        maintype.doc = $1
      else:
        sects.add Sect(kind: skDoc, doc: $1)
    Seq <- K("seq") * I * Array(Attr):
      var sect = Sect(kind: skSeq)
      while attrs.len > 0:
        sect.attrs.add(attrs.pop)
      if level == 0:
        maintype.attrs = sect.attrs
      else:
        sects.add sect
    Insts <- K("instances") * I * Array(Inst) * D
    Enums <- K("enums") * I * Array(Enum) * D
    Key <- App | Consume | Contents | Encoding | Endian | EnumKey |
           EosError | Exts | Id | If | Include | Imports | Io | License |
           Process | Pos | Repeat | RepeatExpr | RepeatUntil | Size | SizeEos |
           Terminator | Title | TypeKey | Value
    SeqKey <- Consume | Contents | Encoding | Endian | EnumKey |
              EosError | Include | Io | Process | Repeat | RepeatExpr |
              RepeatUntil | Size | SizeEos | Terminator | TypeKey
    Attr <- AttrTag * K("id") * >Identifier * +'\n' * Array(SeqKey) * D:
      var attr = Attr(id: $1)
      while keys.len > 0:
        attr.keys.add(keys.pop)
      attrs.add attr
    Type <- K(>Identifier) * I * +(Sect * +'\n') * D:
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
    Inst <- K(>Identifier) * I * Array(Key) * D:
      var i = Inst(name: $1)
      while keys.len > 0:
        i.keys.add(keys.pop)
      insts.add i
    Enum <- K(>Identifier) * I * Array(Pair) * D:
      let e = Enum(name: $1)
      while pairs.len > 0:
        e.pairs.add(pairs.pop)
      enums.add e
    Pair <- K(>+Digit) * >Identifier:
      pairs.add (parseInt($1), $2)

    # Keys
    App <- K("application") * >Line:
      keys.push(kkApp, $1)
    Consume <- K("consume") * >Bool * B:
      keys.push(kkConsume, $1)
    Contents <- K("contents") * (ArrayElem |
                                 ArrayInline(ArrayElem) |
                                 I * YamlArray(ArrayElem) * D):
      var list: seq[byte]
      while elems.len > 0:
        list.stackBytes(elems.pop)
      keys.push(kkContents, blist = list)
    Encoding <- K("encoding") * >Line:
      keys.push(kkEncoding, $1)
    Endian <- K("endian") * >("le" | "be"):
      keys.push(kkEndian, $1)
    Exts <- K("file-extension") *
            (I * YamlArray(FileName) * D | FileName):
      var list: seq[string]
      while elems.len > 0:
        list.insert(elems.pop, 0)
      keys.push(kkExts, slist = list)
    Id <- K("id") * >Identifier:
      keys.push(kkId, $1)
    Imports <- K("imports") *
               (I * YamlArray(FileName) * D | FileName):
      var list: seq[string]
      while elems.len > 0:
        list.insert(elems.pop, 0)
      keys.push(kkImports, slist = list)
    License <- K("license") * >Line:
      keys.push(kkLicense, $1)
    Repeat <- K("repeat") * >("expr" | "eos" | "until"):
      keys.push(kkRepeat, $1)
    Size <- K("size") * >Line:
      keys.push(kkRepeat, $1)
    Title <- K("title") * >Line:
      keys.push(kkTitle, $1)
    TypeKey <- K("type") * >Identifier:
      keys.push(kkType, $1)

    #XXX
    EnumKey <- K("enum") * Line
    EosError <- K("eos-error") * Line
    If <- K("if") * Line # Expression
    Include <- K("include") * Line
    Io <- K("io") * Line
    Process <- K("process") * Line
    Pos <- K("pos") * Line
    RepeatExpr <- K("repeat-expr") * Line # Expression
    RepeatUntil <- K("repeat-until") * Line # Expression
    SizeEos <- K("size-eos") * Line
    Terminator <- K("terminator") * Line
    Value <- K("value") * Line # Expression

  let file = readFile(path)
  doAssert p.match(file).ok

  Ksy(maintype: maintype, types: types, insts: insts, enums: enums)

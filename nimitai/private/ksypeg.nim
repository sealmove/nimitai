import npeg, ksyast, strutils, sequtils, strformat

#[XXX
  doc-ref Sect
  KsyExpression <-
  error handling for enums
]#
type State = object
  maintype: Type
  types: seq[Type]
  insts: seq[Inst]
  enums: seq[Enum]
  sects: seq[seq[Sect]]
  attrs: seq[Attr]
  keys : seq[Key]
  elems: seq[string]
  pairs: seq[tuple[key: int, value: string]]
  level: int
  inds: seq[int]
  root: string
  parents: seq[string]

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
  let p = peg(ksy, state: State):
    # Templates
    K(item) <- item * Colon
    ArrayInline(item) <- '[' * B * item * B * *(',' * B * item * B) * ']'
    ArrItem(item) <- +'\n' * C * item * B
    YArrItem(item) <- +'\n' * C * Tag * item * B
    Array(item) <- +'\n' * I * item * *ArrItem(item) * D
    YArray(item) <- +'\n' * I * Tag * item * *ArrItem(Tag * item) * D

    # Indentation control
    C <- >*' ':
      validate len($1) == state.level
    I <- >*' ':
      let indent = len($1) - state.level
      doAssert indent > 0
      state.inds.add(indent)
      state.level = len($1)
    D <- 0:
      dec(state.level, state.inds.pop)

    # Atoms
    B <- *Blank
    Line <- +(1 - '\n')
    Tag <- '-' * B
    Colon <- B * ':' * B
    Bool <- "true" | "false"
    String <- '"' * *(1 - {'"', '\n'}) * '"'
    Identifier <- {'a'..'z'} * *{'a'..'z','0'..'9','_'}
    Parent <- >Identifier:
      state.parents.add(capitalizeAscii($1))
    ResetParent <- 0:
      discard state.parents.pop
    FileName <- >+{'A'..'Z','a'..'z','0'..'9','_','-','/'}:
      state.elems.add $1
    Item <- >(String | "0x" * +Xdigit | +Digit):
      state.elems.add $1
    AddSectionContainer <- 0:
      state.sects.add(newSeq[Sect]())

    # Main grammar
    ksy <- Sect * +(+'\n' * Sect) * +'\n' * !1:
      state.types.add state.maintype
    Sect <- Meta | Doc | Seq | Types | Insts | Enums
    Types <- K("types") * Array(Type)
    Meta <- K("meta") * Array(Key):
      var sect = Sect(kind: skMeta)
      while state.keys.len > 0:
        sect.keys.add(state.keys.pop)
      if state.level == 0:
        if state.maintype.meta != @[]:
          echo "Meta section is defined more than once"
          quit QuitFailure
        var isId: bool
        for k in sect.keys:
          if k.kind == kkId:
            state.root = k.strval.capitalizeAscii
            state.maintype.root = state.root
            state.maintype.name = state.root
            state.parents.add(state.root)
            isId = true
            state.maintype.meta = sect.keys
            break
        if not isId:
          echo "Missing id in meta section"
          quit QuitFailure
      else:
        state.sects[^1].add sect
    Doc <- K("doc") * >(('|' * B * Array(Line)) | Line):
      if state.level == 0:
        state.maintype.doc = $1
      else:
        state.sects[^1].add Sect(kind: skDoc, doc: $1)
    Seq <- K("seq") * Array(Attr):
      var sect = Sect(kind: skSeq)
      while state.attrs.len > 0:
        sect.attrs.add(state.attrs.pop)
      if state.level == 0:
        state.maintype.attrs = sect.attrs
      else:
        state.sects[^1].add sect
    Insts <- K("instances") * Array(Inst):
      var sect = Sect(kind: skInsts)
      while state.insts.len > 0:
        sect.insts.add(state.insts.pop)
      if state.level == 0:
        state.maintype.insts = sect.insts
      else:
        state.sects[^1].add sect
    Enums <- K("enums") * Array(Enum):
      var sect = Sect(kind: skEnums)
      while state.enums.len > 0:
        sect.enums.add(state.enums.pop)
      if state.level == 0:
        state.maintype.enums = sect.enums
      else:
        state.sects[^1].add sect
    Key <- App | Consume | Contents | Encoding | Endian | EnumKey |
           EosError | Exts | Id | If | Include | Imports | Io | License |
           Process | Pos | Repeat | RepeatExpr | RepeatUntil | Size | SizeEos |
           Terminator | Title | TypeKey | Value
    SeqKey <- Consume | Contents | Encoding | Endian | EnumKey |
              EosError | Include | Io | Process | Repeat | RepeatExpr |
              RepeatUntil | Size | SizeEos | Terminator | TypeKey
    Attr <- Tag * K("id") * >Identifier * Array(SeqKey):
      var attr = Attr(id: $1)
      while state.keys.len > 0:
        attr.keys.add(state.keys.pop)
      state.attrs.add attr
    Type <- K(>Parent) * AddSectionContainer * Array(Sect) * ResetParent:
      var
        t = Type(name: capitalizeAscii($1),
                 root: state.root,
                 parent: state.parents[^1])
        flags: set[SectKind]
        s = $1
      let sections = state.sects.pop
      for sect in sections:
        if flags.contains(sect.kind):
          echo &"{sect.kind} field for {s} declared more than once"
          quit QuitFailure
        flags.incl(sect.kind)
        case sect.kind
        of skMeta:
          t.meta = sect.keys
        of skDoc:
          t.doc = sect.doc
        of skSeq:
          t.attrs = sect.attrs
        of skInsts:
          t.insts = sect.insts
        else:
          discard # If this is reached, something went wrong
      state.types.add t
    Inst <- K(>Identifier) * Array(Key):
      var i = Inst(name: capitalizeAscii($1))
      while state.keys.len > 0:
        i.keys.add(state.keys.pop)
      state.insts.add i
    Enum <- K(>Identifier) * Array(Pair):
      let e = Enum(name: $1)
      while state.pairs.len > 0:
        e.pairs.add(state.pairs.pop)
      state.enums.add e
    Pair <- K(>+Digit) * >Identifier:
      state.pairs.add (parseInt($1), $2)

    # Keys
    App <- K("application") * >Line:
      state.keys.push(kkApp, $1)
    Consume <- K("consume") * >Bool * B:
      state.keys.push(kkConsume, $1)
    Contents <- K("contents") * (Item | ArrayInline(Item) | YArray(Item)):
      var list: seq[byte]
      while state.elems.len > 0:
        list.stackBytes(state.elems.pop)
      state.keys.push(kkContents, blist = list)
    Encoding <- K("encoding") * >Line:
      state.keys.push(kkEncoding, $1)
    Endian <- K("endian") * >("le" | "be"):
      state.keys.push(kkEndian, $1)
    Exts <- K("file-extension") *
            (YArray(FileName) | FileName):
      var list: seq[string]
      while state.elems.len > 0:
        list.insert(state.elems.pop, 0)
      state.keys.push(kkExts, slist = list)
    Id <- K("id") * >Identifier:
      state.keys.push(kkId, $1)
    Imports <- K("imports") * (YArray(FileName) | FileName):
      var list: seq[string]
      while state.elems.len > 0:
        list.insert(state.elems.pop, 0)
      state.keys.push(kkImports, slist = list)
    License <- K("license") * >Line:
      state.keys.push(kkLicense, $1)
    Repeat <- K("repeat") * >("expr" | "eos" | "until"):
      state.keys.push(kkRepeat, $1)
    Size <- K("size") * >Line:
      state.keys.push(kkRepeat, $1)
    Title <- K("title") * >Line:
      state.keys.push(kkTitle, $1)
    TypeKey <- K("type") * >Identifier:
      state.keys.push(kkType, $1)

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

  # Initializations
  let file = readFile(path).splitLines
                           .filterIt(not it.startsWith('#'))
                           .join("\n")
  var state: State
  state.maintype = Type(parent: "RootObj")

  doAssert p.match(file, state).ok

  Ksy(maintype: state.maintype, types: state.types)

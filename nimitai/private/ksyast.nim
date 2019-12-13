import npeg, strutils, sequtils, strformat, tables

type
  State = object
    maintype: Type
    types: seq[Type]
    insts: seq[Inst]
    enums: seq[Enum]
    sects: seq[TableRef[SectKind, Sect]]
    attrs: seq[Attr]
    keys : Table[KeyKind, Key]
    elems: seq[string]
    pairs: Table[string, int]
    level: int
    inds: seq[int]
  Type* = ref object
    name*: string
    parent*: Type
    root*: Type
    meta*: Table[KeyKind, Key]
    doc*: string
    attrs*: seq[Attr]
    insts*: seq[Inst]
    types*: seq[Type]
    enums*: seq[Enum]
  SectKind* = enum
    skMeta
    skDoc
    skSeq
    skTypes
    skInsts
    skEnums
  Sect* = ref object
    case kind*: SectKind
    of skMeta:
      keys*: Table[KeyKind, Key]
    of skDoc:
      doc*: string
    of skSeq:
      attrs*: seq[Attr]
    of skTypes:
      types*: seq[Type]
    of skInsts:
      insts*: seq[Inst]
    of skEnums:
      enums*: seq[Enum]
  Attr* = ref object
    id*: string
    keys*: Table[KeyKind, Key]
  Inst* = ref object
    name*: string
    keys*: Table[KeyKind, Key]
  Enum* = ref object
    name*: string
    pairs*: Table[string, int]
  KeyKind* = enum
    kkApp
    kkConsume
    kkContents
    kkDoc
    kkDocRef
    kkEncoding
    kkEndian
    kkEnum
    kkEosError
    kkExts
    kkId
    kkIf
    kkImports
    kkInclude
    kkIo
    kkLicense
    kkProcess
    kkPos
    kkRepeat
    kkRepeatExpr
    kkRepeatUntil
    kkSize
    kkSizeEos
    kkTerminator
    kkTitle
    kkType
    kkValue
  Key* = ref object
    case kind*: KeyKind
    of kkContents, kkExts, kkImports:
      list*: seq[string]
    of kkApp, kkConsume, kkEndian, kkDoc, kkDocRef, kkEnum, kkEosError,
       kkEncoding, kkId, kkIf, kkInclude, kkIo, kkLicense, kkProcess, kkPos,
       kkRepeat, kkRepeatExpr, kkRepeatUntil, kkSize, kkSizeEos, kkTerminator,
       kkTitle, kkType, kkValue:
      strval*: string

# Debug procs
proc ksyError(msg: string) =
  echo msg
  quit QuitFailure

proc parseKsy*(path: string): Type =
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
    FileName <- >+{'A'..'Z','a'..'z','0'..'9','_','-','/'}:
      state.elems.add $1
    Item <- >(String | "0x" * +Xdigit | +Digit):
      state.elems.add $1
    AddSectionTable <- 0:
      state.sects.add(newTable[SectKind, Sect]())

    # Main grammar
    ksy <- *'\n' * Sect * +(+'\n' * Sect) * +'\n' * !1
    Sect <- Meta | Doc | Seq | Types | Insts | Enums
    Types <- K("types") * Array(Type):
      var sect = Sect(kind: skTypes, types: state.types)
      state.types.setLen(0)
      if state.level == 0:
        for i in 0 ..< sect.types.len:
          sect.types[i].parent = state.maintype
        state.maintype.types = sect.types
      else:
        state.sects[^1][skTypes] = sect
    Meta <- K("meta") * Array(Key):
      var sect = Sect(kind: skMeta, keys: state.keys)
      clear(state.keys)
      if state.level == 0:
        if kkId notin sect.keys:
          ksyError "Missing id in meta section"
        state.maintype.name = sect.keys[kkId].strval
        state.maintype.meta = sect.keys
      else:
        state.sects[^1][skMeta] = sect
    Doc <- K("doc") * >(('|' * B * Array(Line)) | Line):
      if state.level == 0:
        state.maintype.doc = $1
      else:
        state.sects[^1][skDoc] = Sect(kind: skDoc, doc: $1)
    Seq <- K("seq") * Array(Attr):
      var sect = Sect(kind: skSeq, attrs: state.attrs)
      state.attrs.setLen(0)
      if state.level == 0:
        state.maintype.attrs = sect.attrs
      else:
        if skSeq in state.sects[^1]:
          ksyError &"Multiple \"seq\" sections in one type"
        state.sects[^1][skSeq] = sect
    Insts <- K("instances") * Array(Inst):
      var sect = Sect(kind: skInsts, insts: state.insts)
      state.insts.setLen(0)
      if state.level == 0:
        state.maintype.insts = sect.insts
      else:
        state.sects[^1][skInsts] = sect
    Enums <- K("enums") * Array(Enum):
      var sect = Sect(kind: skEnums, enums: state.enums)
      state.enums.setLen(0)
      if state.level == 0:
        state.maintype.enums = sect.enums
      else:
        state.sects[^1][skEnums] = sect
    Key <- App | Consume | Contents | DocKey | DocRefKey | Encoding | Endian |
           EnumKey | EosError | Exts | Id | If | Include | Imports | Io |
           License | Process | Pos | Repeat | RepeatExpr | RepeatUntil | Size |
           SizeEos | Terminator | Title | TypeKey | Value
    Attr <- Tag * K("id") * >Identifier * Array(Key):
      state.attrs.add Attr(id: $1, keys: state.keys)
      clear(state.keys)
    Type <- K(>Identifier) * AddSectionTable * Array(Sect):
      var t = Type(name: $1, root: state.maintype)
      let sections = state.sects.pop
      if skDoc in sections:
        t.doc = sections[skDoc].doc
      if skSeq in sections:
        t.attrs = sections[skSeq].attrs
      if skInsts in sections:
        t.insts = sections[skInsts].insts
      if skTypes in sections:
        for i in 0 ..< sections[skTypes].types.len:
          sections[skTypes].types[i].parent = t
        t.types = sections[skTypes].types
      state.types.add t
    Inst <- K(>Identifier) * Array(Key):
      state.insts.add Inst(name: $1, keys: state.keys)
      clear(state.keys)
    Enum <- K(>Identifier) * Array(Pair):
      state.enums.add Enum(name: $1, pairs: state.pairs)
      clear(state.pairs)
    Pair <- K(>+Digit) * >Identifier:
      state.pairs[$2] = parseInt($1)

    # Keys
    App <- K("application") * >Line:
      state.keys[kkApp] = Key(kind: kkApp, strval: $1)
    Consume <- K("consume") * >Bool * B:
      state.keys[kkConsume] = Key(kind: kkConsume, strval: $1)
    Contents <- K("contents") * (Item | ArrayInline(Item) | YArray(Item)):
      state.keys[kkContents] = Key(kind: kkContents, list: state.elems)
      state.elems.setLen(0)
    DocKey <- K("doc") * >(('|' * B * Array(Line)) | Line):
      state.keys[kkDoc] = Key(kind: kkDoc, strval: $1)
    DocRefKey <- K("doc-ref") * >(('|' * B * Array(Line)) | Line):
      state.keys[kkDocRef] = Key(kind: kkDocRef, strval: $1)
    Encoding <- K("encoding") * >Line:
      state.keys[kkEncoding] = Key(kind: kkEncoding, strval: $1)
    Endian <- K("endian") * >("le" | "be"):
      state.keys[kkEndian] = Key(kind: kkEndian, strval: $1)
    EnumKey <- K("enum") * >Line:
      state.keys[kkEnum] = Key(kind: kkEnum, strval: $1)
    EosError <- K("eos-error") * >Line:
      state.keys[kkEosError] = Key(kind: kkEosError, strval: $1)
    Exts <- K("file-extension") * (YArray(FileName) | FileName):
      state.keys[kkExts] = Key(kind: kkExts, list: state.elems)
      state.elems.setLen(0)
    Id <- K("id") * >Identifier:
      state.keys[kkId] = Key(kind: kkId, strval: $1)
    Imports <- K("imports") * (YArray(FileName) | FileName):
      state.keys[kkImports] = Key(kind: kkImports, list: state.elems)
      state.elems.setLen(0)
    License <- K("license") * >Line:
      state.keys[kkLicense] = Key(kind: kkLicense, strval: $1)
    Repeat <- K("repeat") * >("expr" | "eos" | "until"):
      state.keys[kkRepeat] = Key(kind: kkRepeat, strval: $1)
    Size <- K("size") * >Line:
      state.keys[kkSize] = Key(kind: kkSize, strval: $1)
    Title <- K("title") * >Line:
      state.keys[kkTitle] = Key(kind: kkTitle, strval: $1)
    TypeKey <- K("type") * >Identifier:
      state.keys[kkType] = Key(kind: kkType, strval: $1)
    If <- K("if") * >Line: # Expression
      state.keys[kkIf] = Key(kind: kkIf, strval: $1)
    Include <- K("include") * >Line:
      state.keys[kkInclude] = Key(kind: kkInclude, strval: $1)
    Io <- K("io") * >Line:
      state.keys[kkIo] = Key(kind: kkIo, strval: $1)
    Process <- K("process") * >Line:
      state.keys[kkProcess] = Key(kind: kkProcess, strval: $1)
    Pos <- K("pos") * >Line:
      state.keys[kkPos] = Key(kind: kkPos, strval: $1)
    RepeatExpr <- K("repeat-expr") * >Line: # Expression
      state.keys[kkRepeatExpr] = Key(kind: kkRepeatExpr, strval: $1)
    RepeatUntil <- K("repeat-until") * >Line: # Expression
      state.keys[kkRepeatUntil] = Key(kind: kkRepeatUntil, strval: $1)
    SizeEos <- K("size-eos") * >Line:
      state.keys[kkSizeEos] = Key(kind: kkSizeEos, strval: $1)
    Terminator <- K("terminator") * >Line:
      state.keys[kkTerminator] = Key(kind: kkTerminator, strval: $1)
    Value <- K("value") * >Line: # Expression
      state.keys[kkValue] = Key(kind: kkValue, strval: $1)

  let file = readFile(path).splitLines
                           .filterIt(not it.strip.startsWith('#'))
                           .join("\n")
  var state: State
  state.maintype = Type(parent: Type(name: "RootObj"))
  state.maintype.root = state.maintype
  doAssert p.match(file, state).ok
  state.maintype

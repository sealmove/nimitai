import tables, strutils, macros
import npeg
import lexer, exprlang

type
  Keys* = Table[KeyKind, Key]
  Sects* = Table[SectKind, Sect]
  Type* = tuple[name: string, sects: Sects]
  Types* = Table[string, Sects]
  Inst* = tuple[name: string, keys: Keys]
  Insts* = Table[string, Keys]
  Enum* = tuple[name: string, es: Table[string, int]]
  Enums* = Table[string, Table[string, int]]
  SectKind* = enum
    skMeta
    skDoc
    skDocRef
    skSeq
    skTypes
    skInstances
    skEnums
  Sect* = ref object
    case kind*: SectKind
    of skMeta:
      meta*: Keys
    of skDoc:
      doc*: string
    of skDocRef:
      `doc-ref`*: string
    of skSeq:
      `seq`*: seq[Keys]
    of skTypes:
      types*: Types
    of skInstances:
      instances*: Insts
    of skEnums:
      enums*: Enums
  KeyKind* = enum
    kkApplication
    kkConsume
    kkContents
    kkDoc
    kkDocRef
    kkEncoding
    kkEndian
    kkEnum
    kkEosError
    kkFileExtension
    kkId
    kkIf
    kkImports
    kkInclude
    kkIo
    kkKsDebug
    kkKsOpaqueTypes
    kkKsVersion
    kkLicense
    kkPadRight
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
    of kkApplication, kkDoc, kkDocRef, kkEncoding, kkEndian, kkEnum, kkId,
       kkKsVersion, kkLicense, kkTitle, kkType, kkRepeat:
      item*: string
    of kkContents, kkFileExtension, kkImports, kkProcess:
      items*: seq[string]
    else:
      expr*: Expr
  State = ref object
    itemStack: seq[string]
    exprStack: seq[Expr]
    keyStack:  seq[Key]
    keysStack: seq[Keys]
    sectsStack: seq[Sects]
    typeStack: seq[Type]
    instStack: seq[Inst]
    eStack: seq[tuple[name: string, ordinal: int]]
    enumStack: seq[Enum]

proc `==`(token: Token, tk: TokenKind): bool =
  token.kind == tk

proc parseKsy(tokens: seq[Token]): Sects =
  var
    s = State()
  let parser = peg(G, Token, s: State):
    i(item) <- [tkIndent] * item * [tkDedent]
    Item <- [tkItem]:
      s.itemStack.add ($0).value
    Expr <- Item | i(+Item) * Item:
      let e = s.itemstack.join(" ")
      s.itemStack.setLen(0)
      s.exprStack.add expr(e)

    Array <- +([tkDash] * Item)

    G <- T * !1
    NSS <- 0:
      s.sectsStack.add Sects()
    T <- NSS * +Sect
    Doc <- [tkDoc] * (Item | [tkDocMark] * i(+Item))
    DocRef <- [tkDocRef] * Item
    LabeledType <- >[tkName] * i(T):
      s.typeStack.add (($1).value, s.sectsStack.pop)
    Inst <- >[tkName] * i(+Key):
      var inst: Keys
      for key in s.keyStack:
        inst[key.kind] = key
      s.keyStack.setlen(0)
      s.instStack.add (($1).value, inst)
    E <- >[tkName] * >[tkItem]:
      s.eStack.add (($2).value, parseInt(($1).value))
    Enum <- >[tkName] * i(+E):
      var `enum`: Enum
      `enum`.name = ($1).value
      for e in s.eStack:
        `enum`.es[e.name] = e.ordinal
      s.enumStack.add `enum`
      s.eStack.setlen(0)
    Keys <- [tkDash] * Id * i(*Key):
      var keys: Keys
      for key in s.keyStack:
        keys[key.kind] = key
      s.keyStack.setlen(0)
      s.keysStack.add keys

    Sect <- Meta | DocSect | DocRefSect | Seq | Types | Instances | Enums
    Key <- Application  | Consume | Contents | DocKey | DocRefKey | Encoding |
           Endian | EnumKey | EosError | FileExtension | Id | If | Imports |
           Include | Io | KsDebug | KsOpaqueTypes | KsVersion | License |
           PadRight | Process | Pos | Repeat | RepeatExpr | RepeatUntil | Size |
           SizeEos | Terminator | Title | Type | Value

    Meta <- [tkMeta] * i(+Key):
      var meta: Keys
      for key in s.keyStack:
        meta[key.kind] = key
      s.keyStack.setlen(0)
      let last = s.sectsStack.len - 1
      s.sectsStack[last][skMeta] = Sect(kind: skMeta, meta: meta)
    DocSect <- Doc:
      var doc = s.itemStack.join(" ")
      s.itemStack.setlen(0)
      let last = s.sectsStack.len - 1
      s.sectsStack[last][skDoc] = Sect(kind: skDoc, doc: doc)
    DocRefSect <- DocRef:
      let last = s.sectsStack.len - 1
      s.sectsStack[last][skDocRef] = Sect(kind: skDocRef, `doc-ref`: s.itemStack.pop)
    Seq <- [tkSeq] * i(+Keys):
      let last = s.sectsStack.len - 1
      s.sectsStack[last][skSeq] = Sect(kind: skSeq, `seq`: s.keysStack)
      s.keysStack.setlen(0)
    Types <- [tkTypes] * i(+LabeledType):
      var types: Types
      for t in s.typeStack:
        types[t.name] = t.sects
      let last = s.sectsStack.len - 1
      s.sectsStack[last][skTypes] = Sect(kind: skTypes, types: types)
    Instances <- [tkInstances] * i(+Inst):
      var insts: Insts
      for i in s.instStack:
        insts[i.name] = i.keys
      let last = s.sectsStack.len - 1
      s.sectsStack[last][skInstances] = Sect(kind: skInstances, instances: insts)
      s.instStack.setlen(0)
    Enums <- [tkEnums] * i(+Enum):
      var enums: Enums
      for `enum` in s.enumStack:
        enums[`enum`.name] = `enum`.es
      let last = s.sectsStack.len - 1
      s.sectsStack[last][skEnums] = Sect(kind: skEnums, enums: enums)
      s.enumStack.setlen(0)
    Application <- [tkApplication] * >[tkItem]:
      s.keyStack.add Key(kind: kkApplication, item: ($1).value)
    Consume <- [tkConsume] * Expr:
      s.keyStack.add Key(kind: kkConsume, expr: s.exprStack.pop)
    Contents <- [tkContents] * (Item | Array):
      s.keyStack.add Key(kind: kkContents, items: s.itemStack)
      s.itemStack.setlen(0)
    DocKey <- Doc:
      var doc = s.itemStack.join(" ")
      s.itemStack.setlen(0)
      s.keyStack.add Key(kind: kkDoc, item: doc)
    DocRefKey <- DocRef:
      s.keyStack.add Key(kind: kkDocRef, item: s.itemStack.pop)
    Encoding <- [tkEncoding] * >[tkItem]:
      s.keyStack.add Key(kind: kkEncoding, item: ($1).value)
    Endian <- [tkEndian] * >[tkItem]:
      s.keyStack.add Key(kind: kkEndian, item: ($1).value)
    EnumKey <- [tkEnum] * >[tkItem]:
      s.keyStack.add Key(kind: kkEnum, item: ($1).value)
    EosError <- [tkEosError] * Expr:
      s.keyStack.add Key(kind: kkEosError, expr: s.exprStack.pop)
    FileExtension <- [tkFileExtension] * (Item | Array):
      s.keyStack.add Key(kind: kkFileExtension, items: s.itemStack)
      s.itemStack.setlen(0)
    Id <- [tkId] * >[tkItem]:
      s.keyStack.add Key(kind: kkId, item: ($1).value)
    If <- [tkIf] * Expr:
      s.keyStack.add Key(kind: kkIf, expr: s.exprStack.pop)
    Imports <- [tkImports] * Array:
      s.keyStack.add Key(kind: kkImports, items: s.itemStack)
      s.itemStack.setlen(0)
    Include <- [tkInclude] * Expr:
      s.keyStack.add Key(kind: kkInclude, expr: s.exprStack.pop)
    Io <- [tkIo] * Expr:
      s.keyStack.add Key(kind: kkIo, expr: s.exprStack.pop)
    KsDebug <- [tkKsDebug] * Expr:
      s.keyStack.add Key(kind: kkKsDebug, expr: s.exprStack.pop)
    KsOpaqueTypes <- [tkKsOpaqueTypes] * Expr:
      s.keyStack.add Key(kind: kkKsOpaqueTypes, expr: s.exprStack.pop)
    KsVersion <- [tkKsVersion] * >[tkItem]:
      s.keyStack.add Key(kind: kkKsVersion, item: ($1).value)
    License <- [tkLicense] * >[tkItem]:
      s.keyStack.add Key(kind: kkLicense, item: ($1).value)
    PadRight <- [tkPadRight] * Expr:
      s.keyStack.add Key(kind: kkPadRight, expr: s.exprStack.pop)
    Process <- [tkProcess] * (Item | Array):
      s.keyStack.add Key(kind: kkProcess, items: s.itemStack)
      s.itemStack.setlen(0)
    Pos <- [tkPos] * Expr:
      s.keyStack.add Key(kind: kkPos, expr: s.exprStack.pop)
    Repeat <- [tkRepeat] * >[tkItem]:
      s.keyStack.add Key(kind: kkRepeat, item: ($1).value)
    RepeatExpr <- [tkRepeatExpr] * Expr:
      s.keyStack.add Key(kind: kkRepeatExpr, expr: s.exprStack.pop)
    RepeatUntil <- [tkRepeatUntil] * Expr:
      s.keyStack.add Key(kind: kkRepeatUntil, expr: s.exprStack.pop)
    Size <- [tkSize] * Expr:
      s.keyStack.add Key(kind: kkSize, expr: s.exprStack.pop)
    SizeEos <- [tkSizeEos] * Expr:
      s.keyStack.add Key(kind: kkSizeEos, expr: s.exprStack.pop)
    Terminator <- [tkTerminator] * Expr:
      s.keyStack.add Key(kind: kkTerminator, expr: s.exprStack.pop)
    Title <- [tkTitle] * >[tkItem]:
      s.keyStack.add Key(kind: kkTitle, item: ($1).value)
    Type <- [tkType] * >[tkItem]:
      s.keyStack.add Key(kind: kkType, item: ($1).value)
    Value <- [tkValue] * Expr:
      s.keyStack.add Key(kind: kkValue, expr: s.exprStack.pop)

  assert parser.match(tokens, s).ok
  assert s.sectsStack.len == 1
  s.sectsStack[0]

proc parse*(path: string): Table[SectKind, Sect] = path.tokenizeKsy.parseKsy

proc `$`*(k: Key): string =
  result = $k.kind
  case k.kind
  of kkApplication, kkDoc, kkDocRef, kkEncoding, kkEndian, kkEnum, kkId,
     kkKsVersion, kkLicense, kkTitle, kkType, kkRepeat:
    result &= "(" & k.item & ")"
  of kkFileExtension, kkImports, kkProcess:
    result &= $k.items
  else:
    result &= $k.expr
proc `$`*(keys: Keys): string =
  result &= "KEYS("
  for k in keys.keys:
    result &= $keys[k] & " "
  result &= ")"
proc `$`*(i: Inst): string =
  result &= i.name & "("
  let keys = i.keys
  for k in keys.keys:
    result &= $keys[k] & " "
  result &= ")"
proc `$`*(s: Sect): string =
  result &= $s.kind & "("
  case s.kind
  of skMeta:
    result &= $s.meta
  of skDoc:
    result &= s.doc
  of skDocRef:
    result &= s.`doc-ref`
  of skSeq:
    result &= $s.`seq`
  of skTypes:
    result &= $s.types
  of skInstances:
    result &= $s.instances
  of skEnums:
    result &= "ENUM"
  result &= ")"
proc `$`*(t: Sects): string =
  for k in t.keys:
    result &= $t[k] & "\n"

proc debugParser*(path: string) =
  let tokens = tokenizeKsy(path)
  let ast = parseKsy(tokens)

  echo "=== AST ==="
  echo ast

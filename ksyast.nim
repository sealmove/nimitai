type
  KsyNodeKind* = enum
    knkType
    knkSection
    knkInst
    knkEnum
    knkAttr
  KsyNode* = ref object
    case kind*: KsyNodeKind
    of knkType:
      typeNode*: Type
    of knkSection:
      sectionNode*: Section
    of knkInst:
      instNode*: Inst
    of knkEnum:
      enumNode*: Enum
    of knkAttr:
      attrNode*: Attr
  Type* = ref object
    sections: seq[Section]
  SectionKind* = enum
    skMeta
    skDoc
    skSeq
    skTypes
    skInsts
    skEnums
  Section* = ref object
    case kind*: SectionKind
    of skMeta, skSeq:
      attrs*: seq[Attr]
    of skDoc:
      doc*: string
    of skTypes:
      types*: seq[Type]
    of skInsts:
      insts*: seq[Inst]
    of skEnums:
      enums*: seq[Enum]
  Inst* = ref object
    attrs: seq[Attr]
  Enum* = ref object
    key: int
    value: string
  AttrKind* = enum
    akApp
    akConsume
    akContents
    akEncoding
    akEndian
    #akEnum
    #akEosError
    akExts
    akId
    #akIf
    akImports
    #akInclude
    #akIo
    akLicence
    #akProcess
    #akPos
    akRepeat
    #akRepeatExpr
    #akRepeatUntil
    akSize
    #akSizeEos
    #akTerminator
    akTitle
    akType
    #akValue
  Attr* = ref object
    case kind*: AttrKind
    of akConsume:
      consume*: bool
    of akContents:
      contents*: seq[byte]
    of akEndian:
      endian*: Endian
    #of akEnum
    #of akEosError
    of akExts, akImports:
      list*: seq[string]
    of akApp, akEncoding, akId, akLicence, akTitle, akType:
      strval*: string
    #of akIf
    #of akInclude
    #of akIo
    #of akProcess
    #of akPos
    of akRepeat:
      repeat*: Repeat
    #of akRepeatExpr
    #of akRepeatUntil
    of akSize:
      size*: int64
    #of akSizeEos
    #of akTerminator
    #of akValue:
  Endian* = enum
    le
    be
  Repeat* = enum
    expr
    eos
    until

type
  Type* = ref object
    sections: seq[Section]
  SectionKind: enum
    skMeta
    skDoc
    skSeq
    skTypes
    skInst
    skEnums
  Section* = ref object
    case kind*: SectionKind
    of skMeta: attrs: seq[Attr]
    of skDoc: string
    of skSeq: seq[Attr]
    of skTypes: seq[Type]
    of skInsts: seq[Inst]
    of skEnums: seq[Enum]
  Inst* = ref object
    attrs: seq[Attr]
  Enum* = ref object
    key: int
    value: string

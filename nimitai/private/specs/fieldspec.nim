import ../../runtime

type
  FieldKind* = enum
    fkNone
    fkInteger
    fkArray
    fkStrz
  ArrayKind* = enum
    akNone
    akByte
    akString
  Field* = ref object
    id*: string
    doc*: string
    docRef*: string
    contents*: seq[byte]
    typ*: string
    repeat*: Repeat
    repeatExpr*: Expression
    repeatUntil*: Expression
    ifExpr*: Expression
    case kind*: FieldKind
    of fkInteger:
      label*: string
    of fkArray:
      size*: int64
      sizeEos*: bool
      case arrayKind*: ArrayKind
      of akByte:
        process*: Process
      of akString:
        encoding*: string # might change to a big fat enum
      of akNone: discard
    of fkStrz:
      terminator*: byte
      consume*: bool
      includeTerminator*: bool
      eosError*: bool
    of fkNone: discard
    case isLazy*: bool
    of true:
      pos*: int64
      io: KaitaiStream
      value: int
    of false: discard
  Expression* = ref object
  Repeat* = enum
    rExpr
    rEos
    rUntil
  Process* = enum
    pXor

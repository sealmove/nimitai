type
  Identifier* = distinct string
  TypeId* = ref object
    absolute*: bool
    names*: seq[string]
    isArray*: bool
  ExprKind* = enum
    ekBoolOp
    ekBinOp
    ekUnaryOp
    ekIfExp
    ekCompare
    ekCall
    ekIntNum
    ekFloatNum
    ekStr
    ekBool
    ekEnum
    ekAttribute
    ekCastToType
    ekByteSizeOfType
    ekBitSizeOfType
    ekSubscript
    ekName
    ekList
  EnumKind* = enum
    byLabel
    byId
  Expr* = ref object
    case kind*: ExprKind
    of ekBoolOp:
      boolOp*: BoolOp
      values*: seq[Expr]
    of ekBinOp:
      binLeft*: Expr
      binOp*: Operator
      binRight*: Expr
    of ekUnaryOp:
      unaryOp*: UnaryOp
      operand*: Expr
    of ekIfExp:
      condition*: Expr
      ifTrue*: Expr
      ifFalse*: Expr
    of ekCompare:
      cmpLeft*: Expr
      cmpOp*: CmpOp
      cmpRight*: Expr
    of ekCall:
      fn*: Expr
      args*: seq[Expr]
    of ekIntNum:
      i*: BiggestInt
    of ekFloatNum:
      f*: BiggestFloat
    of ekStr:
      s*: string
    of ekBool:
      b*: bool
    of ekEnum:
      enumName*: Identifier
      inType*: TypeId
      case enumKind*: EnumKind
      of byLabel: label*: Identifier
      of byId: id*: Expr
    of ekAttribute:
      attrValue*: Expr
      attr*: Identifier
    of ekCastToType:
      castedValue*: Expr
      castType*: TypeId
    of ekByteSizeOfType, ekBitSizeOfType:
      typeName*: TypeId
    of ekSubscript:
      sbsValue*: Expr
      idx*: Expr
    of ekName:
      ident*: Identifier
    of ekList:
      elts*: seq[Expr]
  Operator* = enum
    oAdd
    oSub
    oMult
    oDiv
    oMod
    oLShift
    oRShift
    oBitOr
    oBitXor
    oBitAnd
  UnaryOp* = enum
    uoInvert
    uoNot
    uoMinus
  BoolOp* = enum
    boAnd
    boOr
  CmpOp* = enum
    coEq
    coNotEq
    coLt
    coLtE
    coGt
    coGtE

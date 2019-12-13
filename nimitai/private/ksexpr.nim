import npeg, macros

# Procedure that parse a Kaitai Struct expression and produces an equivelant
# Nim expression node
#XXX Make the parsing more correct/robust and add error messages
#[
proc parseKsExpr(expr: string): NimNode =
  let p = peg(kse, e: NimNode):
    Id <- Lower * *(Alnum | '_')
    B <- *Blank
    Int <- ?{'+','-'} * (+Digit | "0x" * +Xdigit | "0b" * +{'0', '1'})
    Float <- Int * '.' * Int * ?('e' * Int)
    Bool <- "true" | "false"
    Array <- '[' * B * Int * *(',' * B * Int * B) * ']'
    String <- '\'' * *(Print - '\'') | '\"' * *(Print - '\"')
    Enum <- Id * *("::" * Id)

  doAssert p.match(expr, e).ok
  e
]#

proc parseKsExpr*(expr: string): NimNode =
  parseExpr(expr)

import strutils

proc normId*(s: string): string =
  result = s
  while result[0] == '_':
    result.delete(0, 0)
  var i = 1
  while i < result.len:
    if result[i] == '_':
      result.delete(i, i)
      result[i] = result[i].toUpperAscii
    inc i

proc isParsedId*(s: string): string =
  "is" & s.normId.capitalizeAscii

proc instId*(s: string): string =
  s.normId & "Inst"

proc variantId*(t, f: string): string =
  t & "_" & f.capitalizeAscii

proc variantDiscrId*(t, f: string): string =
  variantId(t, f) & "Discr"

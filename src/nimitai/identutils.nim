import macros, strutils

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

proc isParsedId*(s: string): NimNode =
  ident("is" & s.normId.capitalizeAscii)

proc instId*(s: string): NimNode =
  ident(s.normId & "Inst")

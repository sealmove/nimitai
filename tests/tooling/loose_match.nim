import sequtils

proc `==`*[T: SomeInteger, U: SomeInteger](x: T, y: U): bool =
  if sizeof(x) > sizeof(y):
    x == T(y)
  else:
    y == U(x)

proc `==`*[T: SomeInteger](x: string, y: seq[T]): bool =
  for (c1, c2) in x.zip(y):
    if c1 != c2.char:
      return false
  return true

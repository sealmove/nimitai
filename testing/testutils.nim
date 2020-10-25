proc `==`*[T: SomeInteger, U: SomeInteger|enum](a: T, b: U): bool = a == T(b)
#proc `==`*[T: SomeInteger, U: SomeInteger](a: seq[T], b: seq[U]): bool =
#  if a.len != b.len:
#    return false
#  for i in 0 ..< a.len:
#    if a[i] != T(b[i]):
#      return false
#  return true

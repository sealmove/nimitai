proc `~=`*[T, U](a: T, b: U): bool =
  when T is U:
    return a == b
  elif T is seq:
    if a.len != b.len:
      return false
    for i in 0 ..< a.len:
      if not a[i] ~= b[i].T:
        return false
    return true
  else:
    return a == b.T

proc `/`*[T: SomeOrdinal, U: SomeOrdinal](a: T, b: U): T =
  when T is U:
    system.`/`(a, b)
  else:
    system.`/`(a, b.T)

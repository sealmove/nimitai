import fenv, options

proc `~=`*[T, U](a: T, b: U): bool =
  when T is U:
    when T is SomeFloat:
      return abs(a - b) <= epsilon(T)
    else:
      return a == b
  elif T is Option:
    return get(a) ~= b
  elif T is seq:
    if a.len != b.len:
      return false
    for i in 0 ..< a.len:
      if not a[i] ~= b[i]:
        return false
    return true
  else:
    return a ~= b.T

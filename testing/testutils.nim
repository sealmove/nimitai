proc `==`*[T: SomeInteger, U: SomeInteger|enum](a: T, b: U): bool = a == T(b)
proc `/`*[T: SomeInteger, U: SomeInteger](a: T, b: U): T = T(a / float(b))

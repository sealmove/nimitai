import nimitai/private/[ksyast, ksypeg, runtime]

proc generateParser*(path: static[string]) =
  let ksy = parseKsy(path)
  echo ksy.maintype.name

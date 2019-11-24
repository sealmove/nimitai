import nimitai/private/[ksyast, ksypeg, runtime]

proc generateParser*(path: static[string]) =
  let mt = parseKsy(path)
  echo mt.name

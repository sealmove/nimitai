import macros, json

proc generateParser(ksj: string): NimNode =
  let json = parseJson(readFile(ksj))
  result = newLetStmt(ident"x", newLit(json["meta"]["id"]))

macro injectParser(ksj: static[string]) =
  result = generateParser(ksj)

proc writeModule(ksj, module: string) =
  writeFile(module, generateParser(ksj).repr)

proc writeDll(ksj, dll: string) = discard

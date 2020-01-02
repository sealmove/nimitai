#!/usr/bin/env nim

let
  kst = "material/kst/" & paramStr(2) & ".kst"
  ksy = "material/ksy/" & paramStr(2) & ".ksy"

echo "=== KST ==="
echo readFile(kst)

echo "=== KSY ==="
echo readFile(ksy)

echo "=== GEN ==="
writeFile("temp.nim", """
import macros, options
import ../nimitai, ../../kaitai_struct_nim_runtime/kaitai_struct_nim_runtime
expandMacros:
  injectParser(""" & "\"" & ksy & "\")")

exec "nim c --hints:off -r temp"

writeFile("temp.nim", """
import strformat
import tooling/[parser, generator]

const
  R = "\e[31;1m"
  G = "\e[32;1m"
  Y = "\e[33;1m"
  B = "\e[34;1m"
  D = "\e[0m"

static:
  let
    name = """ & "\"" & paramStr(2) & "\"\n" & """
    path = "material/kst/" & name & ".kst"

  try:
    var test: NimNode
    writeFile("temp.nim", suite(parseKst(path).test, 2))
    let (ccOut, ccCode) = gorgeEx("nim c --hints:off temp")
    if ccCode != 0:
      echo &"{Y}[CC]{D} {name}\n{ccOut}"
    else:
      let (rcOut, rcCode) = gorgeEx("./temp")
      if rcCode == 2:
        test = parseKst(path).test
        echo &"{G}[OK]{D} {name}"
      else:
        echo &"{B}[RC]{D} {name}\n{rcOut}"
  except:
    echo &"{R}[CE]{D} {name}"
    echo getCurrentExceptionMsg()""")

exec "nim c -r --hints:off temp"

rmFile "temp"
rmFile "temp.nim"

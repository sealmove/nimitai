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
rmFile "temp"
rmFile "temp.nim"

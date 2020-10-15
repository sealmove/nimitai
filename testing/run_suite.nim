import os, osproc, strformat, strutils

# OK: Okay (Test passed)
# FL: Failed (Failed to pass)
# CE: Compile Error (Failed to compile)
# MI: Missing (Test does not exist)

const
  R = "\e[31;1m"
  G = "\e[32;1m"
  Y = "\e[33;1m"
  B = "\e[34;1m"
  D = "\e[0m"

var tests, fl, ce, mi: int

echo &"{B}[Running]{D} Nimitai"

for k, f in walkDir("tests"):
  if k == pcDir: continue
  inc(tests)

  let
    casename = splitFile(f).name
    dir = "tests/compiled/bin"
    exe = dir / casename
    module = &"tests/compiled/{casename}.nim"

  # Check if compile exists
  if not fileExists(module):
    echo &"  {R}[MI]{D} " & casename
    inc(mi)
    continue

  # Try to compile
  let (co, cc) = execCmdEx(&"nim c --hints:off -w:off --outdir:{dir} {module}")
  if cc != 0:
    inc(ce)
    echo &"  {Y}[CE]{D} " & casename
    continue

  # Run
  let (ro, rc) = execCmdEx(&"{exe}")
  if rc != 0:
    echo &"  {B}[FL]{D} " & casename
    inc(fl)
    continue

  echo &"  {G}[OK]{D} " & casename

echo "----------------------------------------\n" &
     &"  {G}[OK]{D} {(tests - mi - ce - fl).intToStr(3)}\n" &
     &"  {B}[FL]{D} {fl.intToStr(3)}\n" &
     &"  {Y}[CE]{D} {ce.intToStr(3)}\n" &
     &"  {R}[MI]{D} {mi.intToStr(3)}\n" &
     "----------------------------------------\n"

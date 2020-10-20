import os, osproc, strformat, strutils, algorithm

# OK: Okay (Test passed)
# FL: Failed (Failed to pass)
# CE: Compile Error (Failed to compile)
# MI: Missing (Test does not exist)

const
  R = "\e[31;1m"
  G = "\e[32;1m"
  Y = "\e[33;1m"
  B = "\e[34;1m"
  M = "\e[35;1m"
  D = "\e[0m"

var
  ok, fl, ce, mi: int
  testFiles: seq[string]

echo &"{B}[Running]{D} Nimitai"

for k, f in walkDir("tests"):
  if k == pcDir: continue
  testFiles.add(f)

sort(testFiles)

for f in testFiles:
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
    echo &"  {M}[CE]{D} " & casename
    inc(ce)
    continue

  # Run
  let (ro, rc) = execCmdEx(&"{exe}")
  if rc != 0:
    echo &"  {B}[FL]{D} " & casename
    inc(fl)
    continue

  echo &"  {G}[OK]{D} " & casename
  inc(ok)

echo "----------------------------------------\n" &
     &"  {G}[OK]{D} {ok.intToStr(3)}\n" &
     &"  {B}[FL]{D} {fl.intToStr(3)}\n" &
     &"  {M}[CE]{D} {ce.intToStr(3)}\n" &
     &"  {R}[MI]{D} {mi.intToStr(3)}\n" &
     "----------------------------------------\n"

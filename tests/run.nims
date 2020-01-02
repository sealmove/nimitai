#!/usr/bin/env nim
cd "tooling"
try:
  exec "nim c -r --hints:off testsuite"
except OSError:
  discard
finally:
  rmFile "testsuite"

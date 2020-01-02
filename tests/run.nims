#!/usr/bin/env -S nim --hints:off
cd "tooling"
try:
  exec "nim c -r --hints:off testsuite"
except OSError:
  discard
finally:
  rmFile "testsuite"

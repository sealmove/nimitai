#!/usr/bin/env -S nim --hints:off
cd "tooling"
exec "nim c --hints:off -r gensuite"
rmFile "gensuite"
rmFile "temp"
rmFile "temp.nim"

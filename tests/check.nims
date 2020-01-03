#!/usr/bin/env nim
cd "tooling"

writeFile("temp.nim", """
import strformat
import parser, generator

const
  R = "\e[31;1m"
  G = "\e[32;1m"
  Y = "\e[33;1m"
  B = "\e[34;1m"
  D = "\e[0m"

static:
  let
    name = """ & "\"" & paramStr(2) & "\"\n" & """
    path = "../material/kst/" & name & ".kst"

  try:
    var test: NimNode
    writeFile("test.nim", suite(parseKst(path).test, 2))
    let (ccOut, ccCode) = gorgeEx("nim c --hints:off test")
    if ccCode != 0:
      echo &"{Y}[CC]{D} {name}\n{ccOut}"
    else:
      let (rcOut, rcCode) = gorgeEx("./test")
      if rcCode == 2:
        test = parseKst(path).test
        echo &"{G}[OK]{D} {name}"
      else:
        echo &"{B}[RC]{D} {name}\n{rcOut}"
  except:
    echo &"{R}[CE]{D} {name}"
    echo getCurrentExceptionMsg()""")

exec "nim c -r --hints:off temp"
echo readFile("test.nim")

rmFile "temp"
rmFile "temp.nim"
rmFile "test"
rmFile "test.nim"

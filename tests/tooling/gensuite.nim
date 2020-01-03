import strutils, strformat, oswalkdir, algorithm
import parser, generator
from os import extractFilename

const
  R = "\e[31;1m"
  G = "\e[32;1m"
  Y = "\e[33;1m"
  B = "\e[34;1m"
  D = "\e[0m"

# [CC] = FAILED TO COMPILE
# [CE] = CODGEN ERROR
# [RC] = RUNTIME CRASH
proc siftedSuite(): string =
  var
    tests: seq[NimNode]
    CC, CE, RC, OK: int
  var paths: seq[string]
  for k, p in walkDir("../material/kst/"):
    if k == pcFile:
      paths.add p
  sort(paths)
  for p in paths:
    var name = extractFilename(p)
    name.removeSuffix(".kst")
    try:
      writeFile("temp.nim", suite(p.parseKst.test, 2))
      let (ccOut, ccCode) = gorgeEx("nim c --hints:off temp")
      if ccCode != 0:
        inc CC
        echo &"{Y}[CC]{D} {name}"
        writeFile(&"../log/cc/{name}", ccOut)
        continue
      let (rcOut, rcCode) = gorgeEx("./temp")
      if rcCode == 2:
        tests.add(p.parseKst.test)
        inc OK
        echo &"{G}[OK]{D} {name}"
      else:
        inc RC
        echo &"{B}[RC]{D} {name}"
        writeFile(&"../log/rc/{name}", rcOut)
    except:
      inc CE
      echo &"{R}[CE]{D} {name}"
      writeFile(&"../log/ce/{name}", getCurrentExceptionMsg())

  echo ""
  echo &"{G}[OK]{D} {OK.intToStr(3)} {B}[RC]{D} {RC.intToStr(3)} " &
       &"{Y}[CC]{D} {CC.intToStr(3)} {R}[CE]{D} {CE.intToStr(3)}"

  suite(tests)

const code = siftedSuite()
writeFile("testsuite.nim", code)

import strformat
import kstparser, generator

const
  R = "\e[31;1m"
  G = "\e[32;1m"
  Y = "\e[33;1m"
  B = "\e[34;1m"
  D = "\e[0m"

static:
  let
    name = "bits_byte_aligned"
    path = "../material/kst/" & name & ".kst"

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
    echo getCurrentExceptionMsg()

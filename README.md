# <p align="center">nimitai</p>

## Introduction
Nimitai is a compile-time parser generator for binary data. It transforms JSON objects into parsing procedures.  
Each input object should describe a binary format according to [Kaitai Struct](https://kaitai.io/) conventions.

| Exported symbol | Production |
|-----------------|------------|
| `macro injectParser(spec: static[JsonNode])` | static library (compile time code embedding) |
| `proc createDynlib(spec: JsonNode, path: string)` | dynamic library |
| `proc outputModule(spec: JsonNode): string` | nim module (source code) |

## Writing specs
Being ubiquitous and easily parsable, JSON is a great base format, and there is even a CT parser [in Nim stdlib](https://nim-lang.org/docs/json.html). However, writing in JSON is not appealing at all. Luckily there are a couple of nice JSON superset, and nimitai can work in conjuction with any CT parser that outputs `JsonNode`. The end-goal is to use nimitai alongside a YAML compiler in order to leverage the excellent [Kaitai Struct IDE](https://ide.kaitai.io/). Sadly, we don't have one that works at compile-time yet. The plan is to refactor [NimYAML](https://github.com/flyx/NimYAML). Nevertheless, nimitai can progress independently.

![Data flow](flow.svg)

## Advantages over Kaitai Struct
1. **High quality idiomatic Nim code generation at the AST level**  
Something I couldn't achieve for Nim with Kaitai Struct because it's made with Java-like languages in mind. After all, Kaitai Struct is not as language-agnostic as it claims to be.
2. **Pluggable spec-as-compiler**  
Instead of being an external compiler, nimitai is a CT library which means you don't have to mess with makefiles or similar mechanisms - everything is done within the language. The moment you tweak your spec, your project that links to it has a brand new compiler! No scripts needed at all.

## Example

### Codegen
buffered_struct.nim (...)
```nim
import json, nimitai, nimitai/runtime
injectParser(parseJson(readFile"buffered_struct.json"))
```

buffered_struct.json
```json
{
  "meta": {
    "id": "buffered_struct",
    "endian": "le"
  },
  "seq": [
    {
      "id": "len1",
      "type": "u4"
    },
    {
      "id": "block1",
      "type": "block",
      "size": "len1"
    },
    {
      "id": "len2",
      "type": "u4"
    },
    {
      "id": "block2",
      "type": "block",
      "size": "len2"
    },
    {
      "id": "finisher",
      "type": "u4"
    }
  ],
  "types": {
    "block": {
      "seq": [
        {
          "id": "number1",
          "type": "u4"
        },
        {
          "id": "number2",
          "type": "u4"
        }
      ]
    }
  }
}
```

generated code
```nim
type
  Buffered_struct = ref object of KaitaiStruct
    parent: KaitaiStruct
    len1: uint32
    block1: Buffered_structblock
    len2: uint32
    block2: Buffered_structblock
    finisher: uint32

  Buffered_structBlock = ref object of KaitaiStruct
    parent: Buffered_struct
    number1: uint32
    number2: uint32

proc read(_: typedesc[Buffered_structBlock]; io: KaitaiStream;
          root: KaitaiStruct; parent: Buffered_struct): Buffered_structBlock
proc read(_: typedesc[Buffered_struct]; io: KaitaiStream; root: KaitaiStruct;
          parent: KaitaiStruct): Buffered_struct

proc read(_: typedesc[Buffered_structBlock]; io: KaitaiStream;
          root: KaitaiStruct; parent: Buffered_struct): Buffered_structBlock =
  template this(): untyped = result
  this = Buffered_structBlock(io: io, parent: parent)
  this.root = if root == nil: this else: root
  let number1 = readu4le(this.io)
  this.number1 = number1
  let number2 = readu4le(this.io)
  this.number2 = number2

proc read(_: typedesc[Buffered_struct]; io: KaitaiStream; root: KaitaiStruct;
          parent: KaitaiStruct): Buffered_struct =
  template this(): untyped = result
  this = Buffered_struct(io: io, parent: parent)
  this.root = if root == nil: this else: root
  let len1 = readu4le(this.io)
  this.len1 = len1
  let block1Raw = readBytes(this.io, int(len1))
  let block1Io = newKaitaiStream(block1Raw)
  let block1 = Buffered_structblock.read(block1io, this.root, this)
  this.block1 = block1
  let len2 = readu4le(this.io)
  this.len2 = len2
  let block2Raw = readBytes(this.io, int(len2))
  let block2Io = newKaitaiStream(block2Raw)
  let block2 = Buffered_structblock.read(block2io, this.root, this)
  this.block2 = block2
  let finisher = readu4le(this.io)
  this.finisher = finisher

proc fromFile(_: typedesc[Buffered_structBlock]; filename: string): Buffered_structBlock =
  read(Buffered_structBlock, newKaitaiFileStream(filename), nil, nil)

proc fromFile(_: typedesc[Buffered_struct]; filename: string): Buffered_struct =
  read(Buffered_struct, newKaitaiFileStream(filename), nil, nil)
```

### API usage
(...) buffered_struct.nim
```nim
let x = BufferedStruct.fromFile("buffered_struct.bin")
echo "Block1, number1: " & toHex(x.block1.number1.int64, 2)
echo "Block1, number2: " & toHex(x.block1.number2.int64, 2)
echo "Block2, number1: " & toHex(x.block2.number1.int64, 2)
echo "Block2, number2: " & toHex(x.block2.number2.int64, 2)
```

buffered_struct.bin (hex view)
```bin
10 00 00 00 42 00 00 00 43 00 00 00 ff ff ff ff
ff ff ff ff 08 00 00 00 44 00 00 00 45 00 00 00
ee 00 00 00
```

output:
```
Block1, number1: 42
Block1, number2: 43
Block2, number1: 44
Block2, number2: 45
```

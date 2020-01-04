# <p align="center">nimitai</p>

## Introduction
Nimitai is a parser generator implemented as a native Nim library.  
It accepts [KSY grammars](https://doc.kaitai.io/ksy_reference.html) which work best for describing binary data structures.  

###### The word *binary* in this context means hard-for-human-to-read

| Exported symbol | Production |
|-----------------|------------|
| `proc writeModule(ksy, module: string)` | nim module (source code) |
| `proc writeDll(ksy, dll: string)` | dynamic library |
| `macro injectParser(ksy: static[string])` | static library (compile time code embedding) |

## Example

hello_world.ksy
```yaml
meta:
  id: buffered_struct
  endian: le
seq:
  - id: len1
    type: u4
  - id: block1
    type: block
    size: len1
  - id: len2
    type: u4
  - id: block2
    type: block
    size: len2
  - id: finisher
    type: u4
types:
  block:
    seq:
      - id: number1
        type: u4
      - id: number2
        type: u4
```
buffered_struct.bin (hex view)
```bin
10 00 00 00 42 00 00 00 43 00 00 00 ff ff ff ff
ff ff ff ff 08 00 00 00 44 00 00 00 45 00 00 00
ee 00 00 00
```
test_nimitai.nim
```nim
import nimitai, kaitai_struct_nim_runtime
injectParser("buffered_struct.ksy")
let x = BufferedStruct.fromFile("buffered_struct.bin")

echo "Block1, number1: " & toHex(x.block1.number1.int64, 2)
echo "Block1, number2: " & toHex(x.block1.number2.int64, 2)
echo "Block2, number1: " & toHex(x.block2.number1.int64, 2)
echo "Block1, number2: " & toHex(x.block2.number2.int64, 2)
```
output:
```
Block1, number1: 42
Block1, number2: 43
Block2, number1: 44
Block1, number2: 45
```
## API
- One procedure -called `fromFile`- is generated.
- The procedure is namespaced under the file format type as written in the top-level meta section.
- The procedure accepts a file path and returns an object.
- The object have one field for each attribute described in the `.ksy` file.
- The object have the following additional fields:
  - `io`: holds the parsing stream
  - `root`: holds a reference to the root object
  - `parent`: holds a reference to the parent object

## Progress
Nimitai is tested against the same material as Kaitai Struct.  
There is a (nim)script which compiles the official `.kst` files into a unittest module.

<pre>
   List view          |         Tree view
----------------------|----------------------------
                      |         132     
                      |         / \     
132 total             |        /   \    
  127 generate        |      127    5
    13 ok             |      /|\        
      2 pass          |     / | \            
      7 fail          |    13 2 112     
    2 runtime error   |   / \            
    112 compile error |  /   \          
  5 codegen error     | 4     9           
</pre>

## Will a `.ksy` file found in the [official KS gallery](https://formats.kaitai.io/) work as is?
**YES**. The official KSY grammar is supported 100%.  
Alternatively, nimitai can be set to accept Nim expressions and types instead of Kaitai Struct ones.

*Toggling this setting will be described in future documentation*

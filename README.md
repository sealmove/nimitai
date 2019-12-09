# <p align="center">nimitai</p>

## Introduction
Nimitai is a generic parser generator implemented as a native Nim library.  
It accepts [KSY grammar](https://doc.kaitai.io/ksy_reference.html) which works best for describing *binary* formats.  

*The word binary in this context means hard-for-human-to-read*

| Exported symbol | Production |
|-----------------|------------|
| `proc writeModule(ksy, module: string)` | nim module (source code) |
| `proc writeDll(ksy, dll: string)` | dynamic library |
| `macro emitParser(ksy: static[string])` | static library (compile time code embedding) |

## Example

hello_world.ksy
```yaml
meta:
  id: hello_world
  file-extension: hw
seq:
  - id: one
    type: u1
```

file.hw (hex view)
```bin
01
```

test_nimitai.nim
```nim
import nimitai

generateParser("hello_world.ksy")

let x = HelloWorld.fromFile("file.hw")

echo x.one
```

output:
```
1
```
## API
- One procedure per `generateParser` call (called `fromFile`) is generated.
- The procedure is namespaced under the file format type as written in the top-level meta section.
- The procedure accepts a file path and returns an object.
- The object have one field for each attribute described in the `.ksy` file.
- The object have the following additional fields:
  - `io`: holds the parsing stream
  - `root`: holds a reference to the root object
  - `parent`: holds a reference to the parent object

## Internals
- An import statement for the *__runtime library__* is generated.
- The `.ksy` file gets parsed into a nim object hierarchy (with [npeg)](https://github.com/zevv/npeg)
- The object hierarchy is transformed into a sequence of nodes -each one representing a concrete type-
- For each concrete type the following are generated:
  - a type declaration
  - a parsing procedure
  - a destructor
- Lastly, `fromFile` proc is generated

*Everything is done at compile-time*

## Will a `.ksy` file found in the [official KS gallery](https://formats.kaitai.io/) work as is?
**YES**. The official KSY grammar is supported 100%.  
Alternatively, nimitai can be set to accept Nim expressions and types instead of Kaitai Struct ones.

*Toggling this setting will be described in future documentation*

## Notes
NimVM on [devel](https://github.com/nim-lang/Nim/tree/devel) uses a 16bit address space which doesn't suffice for this project.  
Zevv tweaked NimVM so that its registers' size can change easily.  
Using [zevv's fork](https://github.com/zevv/Nim/tree/zevv-vmrework) you can run this project already.  
[Araq](https://github.com/Araq) likes the idea so I expect his changes to be merged soon.

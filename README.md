# nimitai | A native file format parser generator

## Introduction & advantages over Kaitai Struct
Nimitai exposes a single procedure which accepts a [KSY grammar](https://doc.kaitai.io/ksy_reference.html) file and generates parsing procedures for the described file format. Essentially it's [Kaitai Struct](https://kaitai.io/) implemented as a Nim macro!

### Advantages:
- Native library (no external compiler needed)
- No file I/O (no modules per parser)
- Automatic parser update on `.ksy` change.

*This allows for better and easier integration of parsers into your project*


## API
- For each (sub)type described in the `.ksy` file, you get a procedure called `fromFile`
- The procedures are namespaced under the name of the type (as in the `.ksy` but capitalized).
- The procedures accept a file path return an object.
- The objects have one field for each attribute described in the `.ksy` file.
- The objects have the following additional fields:
  - `io`: holds the parsing stream
  - `root`: holds a reference to the root object
  - `parent`: holds a reference to the parent object

### Example

hello_world.ksy
```yaml

meta:
  id: hello_world
seq:
  - id: one
    type: u1
```

file.bin
```bin
1\n
```

test_nimitai.nim
```nim
import nimitai

generateParser("hello_world.ksy")

let x = HelloWorld.fromFile("file.bin")

echo x.one
```

output:
```
1
```
## Internals
When `generateParser` gets called:
  - An import statement for the *__runtime library__* is generated.
  - The `.ksy` file gets parsed into a single object representing the file type (with [npeg)](https://github.com/zevv/npeg) 
  - For the file type and for each of its subtypes recursively, the following are generated:
    - a type declaration
    - a parsing procedure
    - a destructor

*Everything is done at compile-time*

## Will a `.ksy` file found in the [official gallery](https://formats.kaitai.io/) work as is?
**YES**. The official KSY grammar is supported 100%.  
Alternatively, nimitai can be set to accept Nim expressions and types instead of Kaitai Struct ones.

*Toggling this setting will be described in future documentation*

## Notes
NimVM on [devel](https://github.com/nim-lang/Nim/tree/devel) uses a 16bit address space which doesn't suffice for this project.  
Zevv tweaked NimVM so that its registers' size can change easily.  
Using [zevv's fork](https://github.com/zevv/Nim/tree/zevv-vmrework) you can run this project already.  
[Araq](https://github.com/Araq) likes the idea so I expect his changes to be merged soon.

# nimitai
## A native file format metaparser

### What is it?
Nimitai exposes a single proc which accepts a [KSY grammar](https://doc.kaitai.io/ksy_reference.html) file and generates a parser for the described file format. Essentially it's [Kaitai Struct](https://kaitai.io/) implemented as a Nim macro:

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

### How does it work?
- [npeg](https://github.com/zevv/npeg) is used to parse a `.ksy` file (special thanks to [zevv](https://github.com/zevv) for this awesome library <3).
- The KSY AST is used to generate procedures for parsing a file into a structured Nim object.

*everything is done at compile-time*

### What does it bring to the table?
Up until now there is no library in any programming language for parsing an arbitary file. If a library for your specific format does not exist in your language, you have to create it. This can either be done **by hand** or by using a **file parser generator** like [Kaitai Struct](https://kaitai.io/) or if your format is relatively simple, serialization programs like [Protocol Buffers](https://developers.google.com/protocol-buffers) can work too.

Nimitai does away with all this machinary! The advantages are several:
- Zero dependence on external compiler tools
- Each time you change the `.ksy` file, the parser is automatically recompiled along with your project
- Better quality of generated code since it's done on AST level

*this allows for better and easier integration of parsers into your project*

### Will a `.ksy` file found in [Kaitai Struct Collection Gallery](https://formats.kaitai.io/) work as is?
Mostly yes. The official KSY grammar will be supported 100%. However, nimitai might have different defaults; for example:
- Nim expressions instead of Kaitai Struct expressions (you will be able to toggle this)

### Notes
NimVM on [devel](https://github.com/nim-lang/Nim/tree/devel) uses a 16bit address space which doesn't suffice for this project.  
Zevv tweaked NimVM so that its registers' size can change easily.  
Using his fork I am able to run this project without any issues. Hopefully his changes will be merged.

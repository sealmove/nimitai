# <p align="center">nimitai</p>

## Introduction
Nimitai is a parser generator for binary data implemented as a native Nim library.  
It accepts nginx-like files that describe how the binary data will be parsed.

* **Inspiration:** Nimitai is 100% inspired by [Kaitai Struct](https://kaitai.io/). Sadly Nim didn't fit in well with it
* **Goal:** Being limited to Nim, Nimitai has much more pragmatic aims compared to Kaitai Struct
* **Justification:** see section 'Update & Retrospection'

| Exported symbol | Production |
|-----------------|------------|
| `proc writeModule(uclPath, module: string)` | nim module (source code) |
| `proc writeDll(uclPath, dll: string)` | dynamic library |
| `macro injectParser(uclPath: static[string])` | static library (compile time code embedding) |

## Example

hello_world.ucl
```yaml
endian = le;

seq = {
  id: len1;
  type: u4;
}
seq = {
  id: block1;
  type: block;
  size: len1;
}
seq = {
  id: len2;
  type: u4;
}
seq = {
  id: block2;
  type: block;
  size: len2;
}
seq = {
  id: finisher;
  type: u4;
}

types = {
  block {
    seq = {
      id = number1;
      type = u4;
    }
    seq = {
      id: number2;
      type: u4;
    }
  }
}
```

hello_world.bin (hex view)
```bin
10 00 00 00 42 00 00 00 43 00 00 00 ff ff ff ff
ff ff ff ff 08 00 00 00 44 00 00 00 45 00 00 00
ee 00 00 00
```
hello_world.nim
```nim
import nimitai, kaitai_struct_nim_runtime
injectParser("hello_world.ucl")
let x = BufferedStruct.fromFile("hello_world.bin")

echo "Block1, number1: " & toHex(x.block1.number1.int64, 2)
echo "Block1, number2: " & toHex(x.block1.number2.int64, 2)
echo "Block2, number1: " & toHex(x.block2.number1.int64, 2)
echo "Block2, number2: " & toHex(x.block2.number2.int64, 2)
```
output:
```
Block1, number1: 42
Block1, number2: 43
Block2, number1: 44
Block2, number2: 45
```

## 8/10/2020: Update & Retrospection
### Retrospection
To start off, let's remember what Nimitai is supposted to bring to the table:
1. **High quality idiomatic Nim code generation at the AST level**  
Something I couldn't achieve for Nim with Kaitai Struct because it's made with Java-like languages in mind. After all, Kaitai Struct is not as language-agnostic as it claims to be.
2. **Pluggable spec-as-compiler**  
The main selling point. Instead of being an external compiler, Nimitai is a CT library which means you don't have to mess with makefiles or similar mechanisms - everything is done within the language. The moment you tweak your spec, your project that links to it has a brand new compiler! No scripts needed at all.
3. **Compatibility with Kaitai Struct specs**  
This is less important, but still counts, mainly because neglecting it means moving away from the Kaitai Struct community.  
Leveraging the already existing specs is not a problem though, since conversion between formats is simple.

That being said, it's been almost a year since the birth of Nimitai and it's still only an idea with zero implementation results. And you know what? The problem is YAML. Screw YAML!

At this point breaking compatibility with Kaitai Struct is a sane compromise. Although it cancels objective 3, good things come out of it:
- Much simpler parsing
- Nim expressions (instead of ad-hoc DSL)

### The obvious question
:unamused: Okay, so which format then if not YAML?  
:neckbeard: The answer is obvious: JSON.  
:unamused: Wut...? Writing the specs is supposed to be convenient! No thanks!  
:neckbeard: Well, stay with me for a moment; We can pour some sugar to it, namely [UCL](https://github.com/vstakhov/libucl)!  

### tl;dr
The new plan is to give up YAML for good and use [UCL](https://github.com/vstakhov/libucl) instead. This breaks compatibility with Kaitai Struct, but it radically simplifies parsing, brings greater compatibility with JSON, and the syntax is almost as convenient.

## API
**TODO**

## How to leverage [Kaitai Struct's gallery](https://formats.kaitai.io/)
**TODO**

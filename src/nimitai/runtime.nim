import
  streams, endians, sequtils, bitops, strutils, strformat, encodings, algorithm,
  math

type
  KaitaiStruct* {.inheritable.} = ref object
    io*: KaitaiStream
    root*: KaitaiStruct
  KaitaiStream* = ref object
    io: Stream
    bits: uint64
    bitsLeft: int
  KaitaiError* = object of Defect

proc toString(bytes: seq[byte]): string =
  result = newStringOfCap(len(bytes))
  for b in bytes:
    add(result, char(b))

proc newKaitaiFileStream*(f: File): KaitaiStream =
  KaitaiStream(io: newFileStream(f), bits: 0, bitsLeft: 0)
proc newKaitaiFileStream*(filename: string): KaitaiStream =
  KaitaiStream(io: newFileStream(filename), bits: 0, bitsLeft: 0)
proc newKaitaiStream*(data: seq[byte]): KaitaiStream =
  KaitaiStream(io: newStringStream(toString(data)), bits: 0, bitsLeft: 0)
proc newKaitaiStream*(data: seq[seq[byte]]): KaitaiStream =
  KaitaiStream(io: newStringStream(data.mapIt(it.toString).join("")),
               bits: 0, bitsLeft: 0)

# Stream positioning
proc close*(ks: KaitaiStream) = close(ks.io)
proc eof*(ks: KaitaiStream): bool = atEnd(ks.io) and ks.bitsLeft == 0
proc seek*(ks: KaitaiStream, n: int) = setPosition(ks.io, n)
proc pos*(ks: KaitaiStream): int = getPosition(ks.io)
proc skip*(ks: KaitaiStream, n: int) = ks.seek(pos(ks) + n)
proc size*(ks: KaitaiStream): int =
  let p = getPosition(ks.io)
  result = readAll(ks.io).len + p
  setPosition(ks.io, p)

# Signed integer numbers
proc readS1*(ks: KaitaiStream): int8 = readInt8(ks.io)

when system.cpuEndian == bigEndian:
  proc readS2Be*(ks: KaitaiStream): int16 = readInt16(ks.io)
  proc readS4Be*(ks: KaitaiStream): int32 = readInt32(ks.io)
  proc readS8Be*(ks: KaitaiStream): int64 = readInt64(ks.io)

  proc readS2Le*(ks: KaitaiStream): int16 =
    var
      bufferIn: array[2, byte]
      bufferOut: array[2, byte]
    doAssert ks.io.readData(addr(bufferIn), 2) == 2
    swapEndian16(addr(bufferOut), addr(bufferIn))
    result = cast[int16](bufferOut)

  proc readS4Le*(ks: KaitaiStream): int32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[int32](bufferOut)

  proc readS8Le*(ks: KaitaiStream): int64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[int64](bufferOut)
else:
  proc readS2Be*(ks: KaitaiStream): int16 =
    var
      bufferIn: array[2, byte]
      bufferOut: array[2, byte]
    doAssert ks.io.readData(addr(bufferIn), 2) == 2
    swapEndian16(addr(bufferOut), addr(bufferIn))
    result = cast[int16](bufferOut)

  proc readS4Be*(ks: KaitaiStream): int32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[int32](bufferOut)

  proc readS8Be*(ks: KaitaiStream): int64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[int64](bufferOut)

  proc readS2Le*(ks: KaitaiStream): int16 = readInt16(ks.io)
  proc readS4Le*(ks: KaitaiStream): int32 = readInt32(ks.io)
  proc readS8Le*(ks: KaitaiStream): int64 = readInt64(ks.io)

# Unsigned integer numbers
proc readU1*(ks: KaitaiStream): uint8 = readUint8(ks.io)

when system.cpuEndian == bigEndian:
  proc readU2Be*(ks: KaitaiStream): uint16 = readUint16(ks.io)
  proc readU4Be*(ks: KaitaiStream): uint32 = readUint32(ks.io)
  proc readU8Be*(ks: KaitaiStream): uint64 = readUint64(ks.io)

  proc readU2Le*(ks: KaitaiStream): uint16 =
    var
      bufferIn: array[2, byte]
      bufferOut: array[2, byte]
    doAssert ks.io.readData(addr(bufferIn), 2) == 2
    swapEndian16(addr(bufferOut), addr(bufferIn))
    result = cast[uint16](bufferOut)

  proc readU4Le*(ks: KaitaiStream): uint32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[uint32](bufferOut)

  proc readU8Le*(ks: KaitaiStream): uint64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[uint64](bufferOut)
else:
  proc readU2Be*(ks: KaitaiStream): uint16 =
    var
      bufferIn: array[2, byte]
      bufferOut: array[2, byte]
    doAssert ks.io.readData(addr(bufferIn), 2) == 2
    swapEndian16(addr(bufferOut), addr(bufferIn))
    result = cast[uint16](bufferOut)

  proc readU4Be*(ks: KaitaiStream): uint32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[uint32](bufferOut)

  proc readU8Be*(ks: KaitaiStream): uint64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[uint64](bufferOut)

  proc readU2Le*(ks: KaitaiStream): uint16 = readUint16(ks.io)
  proc readU4Le*(ks: KaitaiStream): uint32 = readUint32(ks.io)
  proc readU8Le*(ks: KaitaiStream): uint64 = readUint64(ks.io)

# Floating point numbers
when system.cpuEndian == bigEndian:
  proc readF4Be*(ks: KaitaiStream): float32 = readFloat32(ks.io)
  proc readF8Be*(ks: KaitaiStream): float64 = readFloat64(ks.io)

  proc readF4Le*(ks: KaitaiStream): float32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[float32](bufferOut)

  proc readF8Le*(ks: KaitaiStream): float64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[float64](bufferOut)
else:
  proc readF4Be*(ks: KaitaiStream): float32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[float32](bufferOut)

  proc readF8Be*(ks: KaitaiStream): float64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[float64](bufferOut)

  proc readF4Le*(ks: KaitaiStream): float32 = readFloat32(ks.io)
  proc readF8Le*(ks: KaitaiStream): float64 = readFloat64(ks.io)

# Unaligned bit values
proc align_to_byte*(ks: KaitaiStream) =
  ks.bits = 0
  ks.bitsLeft = 0

proc getMaskOnes(n: int): uint64 =
  if n == 64: 0xFFFFFFFFFFFFFFFF'u64
  else: (1'u64 shl n) - 1

proc readBitsIntBe*(ks: KaitaiStream, n: int): uint64 =
  let bitsNeeded = n - ks.bitsLeft
  if bitsNeeded > 0:
    var bytesNeeded = ((bitsNeeded - 1) div 8) + 1;
    var buf: array[8, byte]
    doAssert ks.io.readData(addr(buf), bytesNeeded) == bytesNeeded
    for i in 0..<bytesNeeded:
      ks.bits = ks.bits shl 8
      ks.bits = ks.bits or buf[i]
      inc(ks.bitsLeft, 8)
  let
    mask = getMaskOnes(n)
    shiftBits = ks.bitsLeft - n
  result = (ks.bits shr shiftBits) and mask
  dec(ks.bitsLeft, n)
  ks.bits = ks.bits and getMaskOnes(ks.bitsLeft)

proc readBitsInt*(ks: KaitaiStream, n: int): uint64 {.deprecated: "use readBitsIntBe instead".} =
  ks.readBitsIntBe(n)

proc readBitsIntLe*(ks: KaitaiStream, n: int): uint64 =
  let bitsNeeded = n - ks.bitsLeft
  if bitsNeeded > 0:
    var bytesNeeded = ((bitsNeeded - 1) div 8) + 1;
    var buf: array[8, byte]
    doAssert ks.io.readData(addr(buf), bytesNeeded) == bytesNeeded
    for i in 0..<bytesNeeded:
      ks.bits = ks.bits or (uint64(buf[i]) shl ks.bitsLeft)
      inc(ks.bitsLeft, 8)
  # raw mask with required number of 1s, starting from lowest bit
  let mask = getMaskOnes(n)
  # derive reading result
  result = ks.bits and mask
  # remove bottom bits that we've just read by shifting
  ks.bits = ks.bits shr n
  dec(ks.bitsLeft, n)

# XXX: proc readBitsArray*(ks: KaitaiStream, n: int): string =

# Byte arrays
proc readBytes*(ks: KaitaiStream, n: int): seq[byte] =
  if n == 0: return
  result = newSeq[byte](n)
  doAssert ks.io.readData(addr(result[0]), n) == n

proc readBytesFull*(ks: KaitaiStream): seq[byte] =
  const bufferSize = 1024
  var buffer {.noinit.}: array[bufferSize, byte]
  while true:
    let bytesRead = ks.io.readData(addr(buffer[0]), bufferSize)
    if bytesRead == 0: break
    let prevLen = result.len
    result.setLen(prevLen + bytesRead)
    copyMem(addr(result[prevLen]), addr(buffer[0]), bytesRead)
    if bytesRead < bufferSize:
      break

proc readBytesTerm*(ks: KaitaiStream; term: byte;
                    includeTerm, consumeTerm, eosError: bool): seq[byte] =
  while true:
    let c = readUint8(ks.io)
    if c == term:
      if includeTerm:
        result.add(term)
      if not consumeTerm:
        ks.io.setPosition(ks.io.getPosition - 1)
      break
    result.add(c)

proc ensureFixedContents*(ks: KaitaiStream, expected: seq[byte]): seq[byte] =
  result = ks.read_bytes(expected.len)
  if result != expected:
    raise newException(KaitaiError, "the request to the OS failed")

proc bytesStripRight*(bytes: seq[byte], padByte: byte): seq[byte] =
  var newLen = bytes.len
  while newLen > 0 and bytes[newLen - 1] == padByte: dec(newLen)
  result = bytes
  result.setLen(newLen)

proc bytesTerminate*(bytes: seq[byte], term: byte,
                     includeTerm: bool): seq[byte] =
  var newLen: int
  let maxLen = bytes.len
  while newLen < maxLen and bytes[newLen] != term: inc(newLen)
  if includeTerm and newLen < maxLen: inc(newLen)
  result = bytes
  result.setLen(newLen)

# XXX: proc bytesToStr(bytes: seq[byte], encoding: string): string =

proc encode*(src: seq[byte], encoding: string): string =
  convert(src.toString, srcEncoding = encoding)

# Byte array processing
proc processXor*(data: seq[byte], key: byte): seq[byte] =
  result = data.mapIt(it xor key)

proc processXor*(data, key: seq[byte]): seq[byte] =
  result = newSeq[byte](data.len)
  var currKeyIdx: int
  for i in 0..<data.len:
    result[i] = data[i] xor key[currKeyIdx]
    inc currKeyIdx
    if currKeyIdx >= key.len: currKeyIdx = 0

proc processRotateLeft*(data: seq[byte], amount: int): seq[byte] =
  result = data.mapIt(rotateLeftBits(it, amount))

# XXX: proc process_zlib(data: seq[byte]): seq[byte] =

proc parseInt*(s: string, radix: int): int {.raises: [ValueError].} =
  case radix
  of 2: parseBinInt(s)
  of 8: parseOctInt(s)
  of 10: parseInt(s)
  of 16: parseHexInt(s)
  else:
    raise newException(ValueError,
      fmt"base {radix} is not supported; use base 2, 8, 10 or 16")

# Custom operators
proc `ksdiv`*[T, U](x: T, y: U): auto =
  when T is float or U is float:
    x.float / y.float
  else:
    floorDiv(x, y.T)
proc `ksmod`*[T: SomeNumber](x, y: T): T = floorMod(x, y)
proc `<=`*(x, y: seq[byte]): bool = x.toString <= y.toString
proc `<`*(x, y: seq[byte]): bool = x.toString <= y.toString
proc `+`*(x, y: string): string = x & y
proc `+`*[T: SomeSignedInt, U: SomeUnsignedInt](x: T, y: U): T = x + T(y)
proc `+`*[U: SomeSignedInt, T: SomeUnsignedInt](x: T, y: U): U = U(x) + y
proc `-`*(n: byte): byte = byte(255'u8 - n + 1)
#proc `%%%`*[T, U: SomeInteger](a: T, b: U): U =
#  if a >= T(0):
#    result = a.U mod b;
#  else:
#    let x = if b >= U(0): b else: -b
#    result = x - 1 + U(a + 1) mod b;


# Expression language methods
# Integers
proc toS*(i: SomeInteger): string = intToStr(int(i))

# Floating point numbers
proc toI*(f: float): int = int(f)

# Byte arrays
proc length*(ba: seq[byte]): int = ba.len

proc toS*(ba: seq[byte], encoding: string): string =
  convert(ba.toString, srcEncoding = encoding)

# Strings
proc length*(s: string): int = s.len
proc reverse*(s: string): string =
  var s = s
  algorithm.reverse(s)
  s
proc substring*(s: string; `from`, to: int): string = substr(s, `from`, to - 1)
proc toI*(s: string): int = parseInt(s)
proc toI*(s: string, radix: int): int =
  case radix
  of  2: parseBinInt(s)
  of  8: parseOctInt(s)
  of 10: parseInt(s)
  of 16: parseHexInt(s)
  else: quit("Radix is not supported")

# Enums
proc toI*(e: enum): int = ord(e)

# Booleans
proc toI*(b: bool): int = ord(b)

# Array
proc first*[T](a: openArray[T]): T = a[0]
proc last*[T](a: openArray[T]): T = a[^1]
proc size*[T](a: openArray[T]): int = len(a)

import streams, endians, sequtils, bitops, strutils, strformat

type
  KaitaiStream* = ref object
    io*: Stream
    bits*: uint64
    bitsLeft*: int

proc newKaitaiStream*(f: File): owned KaitaiStream =
  KaitaiStream(io: newFileStream(f), bits: 0, bitsLeft: 0)
proc newKaitaiStream*(filename: string): owned KaitaiStream =
  KaitaiStream(io: newFileStream(filename), bits: 0, bitsLeft: 0)
proc newKaitaiStream*(data: seq[byte]): owned KaitaiStream =
  KaitaiStream(io: newStringStream(join(data)), bits: 0, bitsLeft: 0)

# Stream positioning
proc close*(ks: KaitaiStream) = close(ks.io)
proc eof*(ks: KaitaiStream): bool = atEnd(ks.io)
proc seek*(ks: KaitaiStream, n: int) = setPosition(ks.io, n)
proc pos*(ks: KaitaiStream): int = getPosition(ks.io)
proc skip*(ks: KaitaiStream, n: int) = ks.seek(pos(ks) + n)
proc size*(ks: KaitaiStream): int =
  let p = getPosition(ks.io)
  result = readAll(ks.io).len
  setPosition(ks.io, p)

# Signed integer numbers
proc read_s1*(ks: KaitaiStream): int8 = readInt8(ks.io)

when system.cpuEndian == bigEndian:
  proc read_s2be*(ks: KaitaiStream): int16 = readInt16(ks.io)
  proc read_s4be*(ks: KaitaiStream): int32 = readInt32(ks.io)
  proc read_s8be*(ks: KaitaiStream): int64 = readInt64(ks.io)

  proc read_s2le*(ks: KaitaiStream): int16 =
    var
      bufferIn: array[2, byte]
      bufferOut: array[2, byte]
    doAssert ks.io.readData(addr(bufferIn), 2) == 2
    swapEndian16(addr(bufferOut), addr(bufferIn))
    result = cast[int16](bufferOut)

  proc read_s4le*(ks: KaitaiStream): int32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[int32](bufferOut)

  proc read_s8le*(ks: KaitaiStream): int64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[int64](bufferOut)
else:
  proc read_s2be*(ks: KaitaiStream): int16 =
    var
      bufferIn: array[2, byte]
      bufferOut: array[2, byte]
    doAssert ks.io.readData(addr(bufferIn), 2) == 2
    swapEndian16(addr(bufferOut), addr(bufferIn))
    result = cast[int16](bufferOut)

  proc read_s4be*(ks: KaitaiStream): int32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[int32](bufferOut)

  proc read_s8be*(ks: KaitaiStream): int64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[int64](bufferOut)

  proc read_s2le*(ks: KaitaiStream): int16 = readInt16(ks.io)
  proc read_s4le*(ks: KaitaiStream): int32 = readInt32(ks.io)
  proc read_s8le*(ks: KaitaiStream): int64 = readInt64(ks.io)

# Unsigned integer numbers
proc read_u1*(ks: KaitaiStream): uint8 = readUint8(ks.io)

when system.cpuEndian == bigEndian:
  proc read_u2be*(ks: KaitaiStream): uint16 = readUint16(ks.io)
  proc read_u4be*(ks: KaitaiStream): uint32 = readUint32(ks.io)
  proc read_u8be*(ks: KaitaiStream): uint64 = readUint64(ks.io)

  proc read_u2le*(ks: KaitaiStream): uint16 =
    var
      bufferIn: array[2, byte]
      bufferOut: array[2, byte]
    doAssert ks.io.readData(addr(bufferIn), 2) == 2
    swapEndian16(addr(bufferOut), addr(bufferIn))
    result = cast[uint16](bufferOut)

  proc read_u4le*(ks: KaitaiStream): uint32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[uint32](bufferOut)

  proc read_u8le*(ks: KaitaiStream): uint64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[uint64](bufferOut)
else:
  proc read_u2be*(ks: KaitaiStream): uint16 =
    var
      bufferIn: array[2, byte]
      bufferOut: array[2, byte]
    doAssert ks.io.readData(addr(bufferIn), 2) == 2
    swapEndian16(addr(bufferOut), addr(bufferIn))
    result = cast[uint16](bufferOut)

  proc read_u4be*(ks: KaitaiStream): uint32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[uint32](bufferOut)

  proc read_u8be*(ks: KaitaiStream): uint64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[uint64](bufferOut)

  proc read_u2le*(ks: KaitaiStream): uint16 = readUint16(ks.io)
  proc read_u4le*(ks: KaitaiStream): uint32 = readUint32(ks.io)
  proc read_u8le*(ks: KaitaiStream): uint64 = readUint64(ks.io)

# Floating point numbers
when system.cpuEndian == bigEndian:
  proc read_f4be*(ks: KaitaiStream): float32 = readFloat32(ks.io)
  proc read_f8be*(ks: KaitaiStream): float64 = readFloat64(ks.io)

  proc read_f4le*(ks: KaitaiStream): float32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[float32](bufferOut)

  proc read_f8le*(ks: KaitaiStream): float64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[float64](bufferOut)
else:
  proc read_f4be*(ks: KaitaiStream): float32 =
    var
      bufferIn: array[4, byte]
      bufferOut: array[4, byte]
    doAssert ks.io.readData(addr(bufferIn), 4) == 4
    swapEndian32(addr(bufferOut), addr(bufferIn))
    result = cast[float32](bufferOut)

  proc read_f8be*(ks: KaitaiStream): float64 =
    var
      bufferIn: array[8, byte]
      bufferOut: array[8, byte]
    doAssert ks.io.readData(addr(bufferIn), 8) == 8
    swapEndian64(addr(bufferOut), addr(bufferIn))
    result = cast[float64](bufferOut)

  proc read_f4le*(ks: KaitaiStream): float32 = readFloat32(ks.io)
  proc read_f8le*(ks: KaitaiStream): float64 = readFloat64(ks.io)

# Unaligned bit values
proc align_to_byte*(ks: KaitaiStream) =
  ks.bits = 0
  ks.bitsLeft = 0

proc read_bits_int*(ks: KaitaiStream, n: int): uint64 =
  proc getMaskOnes(n: int): uint64 =
    if n == 64: 0xFFFFFFFFFFFFFFFF'u64
    else: (1'u64 shl n) - 1
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
    shiftBits = ks.bitsLeft - n
    mask = getMaskOnes(n) shl shiftBits
  result = (ks.bits and mask) shr shiftBits
  dec(ks.bitsLeft, n)
  ks.bits = ks.bits and getMaskOnes(ks.bitsLeft)

# XXX: proc read_bits_array*(ks: KaitaiStream, n: int): seq[byte] =

# Byte arrays
proc read_bytes*(ks: KaitaiStream, n: int): seq[byte] =
  result = newSeq[byte](n)
  doAssert ks.io.readData(addr(result[0]), n) == n

proc read_bytes_full*(ks: KaitaiStream): seq[byte] =
  const bufferSize = 1024
  var buffer {.noinit.}: array[bufferSize, char]
  while true:
    let bytesRead = ks.io.readData(addr(buffer[0]), bufferSize)
    if bytesRead == 0: break
    let prevLen = result.len
    result.setLen(prevLen + bytesRead)
    copyMem(addr(result[prevLen]), addr(buffer[0]), bytesRead)
    if bytesRead < bufferSize:
      break

proc read_bytes_term*(ks: KaitaiStream; term: byte;
                      includeTerm, consumeTerm: bool): seq[byte] =
  while true:
    let c = readUint8(ks.io)
    if c == term:
      if includeTerm:
        result.add(term)
      if not consumeTerm:
        ks.io.setPosition(ks.io.getPosition - 1)
      break
    result.add(c)

proc ensure_fixed_contents*(ks: KaitaiStream, expected: seq[byte]): seq[byte] =
  result = ks.read_bytes(expected.len)
  if result != expected:
    raise newException(AssertionError, "the request to the OS failed")

proc bytes_strip_right*(bytes: seq[byte], padByte: byte): seq[byte] =
  var newLen = bytes.len
  while newLen > 0 and bytes[newLen - 1] == padByte: dec(newLen)
  result = bytes
  result.setLen(newLen)

proc bytes_terminate*(bytes: seq[byte],
                      term: byte, includeTerm: bool): seq[byte] =
  var newLen: int
  let maxLen = bytes.len
  while newLen < maxLen and bytes[newLen] != term: inc(newLen)
  if includeTerm and newLen < maxLen: inc(newLen)
  result = bytes
  result.setLen(newLen)

# XXX: proc bytes_to_str(bytes: seq[byte], encoding: string): string =

# Byte array processing
proc process_xor*(data: seq[byte], key: byte): seq[byte] =
  result = data.mapIt(it xor key)

proc process_xor*(data, key: seq[byte]): seq[byte] =
  result = newSeq[byte](data.len)
  var currKeyIdx: int
  for i in 0..<data.len:
    result[i] = data[i] xor key[currKeyIdx]
    inc currKeyIdx
    if currKeyIdx >= key.len: currKeyIdx = 0

proc process_rotate_left*(data: seq[byte], amount: int): seq[byte] =
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

proc `%%%`*[T, U: SomeInteger](a: T, b: U): U =
  if a >= T(0):
    result = a.U mod b;
  else:
    let x = if b >= U(0): b else: -b
    result = x - 1 + U(a + 1) mod b;

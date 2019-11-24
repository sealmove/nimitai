meta:
  id: foo_arc
  title: Foo Archive
  application: Foo Archiver v1.23
  file-extension:
    - fooarc
    - fooarcz
  license: CC0-1.0
  imports:
    - common/archive_header
    - common/compressed_file
  encoding: UTF-8
  endian: le
doc: |
  A variable-length unsigned integer using base128 encoding. 1-byte groups
  consists of 1-bit flag of continuation and 7-bit value, and are ordered
  "most significant group first", i.e. in "big-endian" manner.

  This particular encoding is specified and used in:

  * Standard MIDI file format
  * ASN.1 BER encoding
seq:
  - id: block2
    type: block
    size: len2
  - id: files
    type: file
  - id: mystring
    type: str
    consume: true
    contents:
      - 1
      - "ABC"
      - 3
      - 4
      - 5
    repeat: eos
types:
  block:
    seq:
      - id: number1
        type: u4
instances:
  myglobalinst:
    value: 6
enums:
  protocol_enum:
    0: hopopt
    1: icmp

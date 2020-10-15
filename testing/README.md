# Testing infrastructure for nimitai

The suite is copied and adjusted from the Kaitai Struct project:
* `subjects/`: mere copy of [`kaitai-io/kaitai_struct_tests/src/`](https://github.com/kaitai-io/kaitai_struct_tests/tree/master/src)
* `specs/`: json equivelant of [`kaitai-io/kaitai_struct_tests/formats/`](https://github.com/kaitai-io/kaitai_struct_tests/tree/master/formats)
* `tests/`: json equivelant of [`kaitai-io/kaitai_struct_tests/spec/ks/`](https://github.com/kaitai-io/kaitai_struct_tests/tree/master/spec/ks)

Periodically these files will be updated with new adjusted copies from Kaitai Struct.

Two tools are included:
- `gen_suite`: Tries to generate `tests/compiled/*nim `from `tests/*.kst`
- `run_suite`: Compiles and runs all generated `unittest`s
Both programs must be run from the same directory where they resides

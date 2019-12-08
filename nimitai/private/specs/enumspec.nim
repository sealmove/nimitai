import tables

type
  Enum* = ref object
    name*: string
    pairs*: Table[string, int]

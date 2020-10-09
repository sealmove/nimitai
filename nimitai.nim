import json

macro injectParser(spec: JsonNode) = discard # static library
proc writeDll(spec: JsonNode, path: string) = discard # dynamic library
proc outputModule(spec: JsonNode): string = discard # source code

import json, npeg, strutils, tables

type ParseInfo = object
  ps: seq[JsonNode]
  ne: seq[int]

proc parseJson6*(s: string): JsonNode =
  let p = peg("doc", pi: ParseInfo):
    indent      <- 0:
      pi.ne.add(0)
    S          <- *Space
    jtrue      <- "true":
      pi.ps.add(newJBool(true))
    jfalse     <- "false":
      pi.ps.add(newJBool(false))
    jnull      <- "null":
      pi.ps.add(newJNull())

    uEscape    <- 'u' * Xdigit[4]
    escape     <- '\\' * ({'{','"','|','\\','b','f','n','r','t'} | uEscape)
    stringBody <- ?escape * *(+({'\x20'..'\xff'} - {'"'} - {'\\'}) * *escape)
    jstring    <- '"' * >stringBody * '"':
      pi.ps.add(newJString($1))

    jfloat   <- ?jintP * '.' * ?(jintP * ?('e' * ?{'+','-'} * jintP)):
      pi.ps.add(newJFloat(parseFloat($0)))
    jintP    <- jhexP | joctP | jbinP | jdecP
    jhexP    <- ?{'+','-'} * "0x" * +(Xdigit | '_')
    joctP    <- ?{'+','-'} * "0o" * +{'0'..'7','_'}
    jbinP    <- ?{'+','-'} * "0b" * +{'0','1','_'}
    jdecP    <- ?{'+','-'} * +(Digit | '_')

    jint <- jhex | joct | jbin | jdec
    jhex <- jhexP:
      pi.ps.add(newJInt(parseHexInt($0)))
    joct <- joctP:
      pi.ps.add(newJInt(parseOctInt($0)))
    jbin <- jbinP:
      pi.ps.add(newJInt(parseBinInt($0)))
    jdec <- jdecP:
      pi.ps.add(newJInt(parseInt($0)))

    jid        <- Alpha * +('_' | Alnum):
      pi.ps.add(newJString($0))
    doc        <- indent * JSON * !1
    jkey       <- jstring | jid:
      inc(pi.ne[^1])
    JSON       <- S * (jfloat | jint | jobject | jarray | jstring | jtrue |
                       jfalse | jnull) * S:
      inc(pi.ne[^1])
    jobject    <- '{' * indent * S * jkey * S * ':' * JSON *
                  *(',' * S * jkey * S * ":" * JSON) * ?(',' * S) * '}':
      var obj = newJObject()
      let n = pi.ne.pop()
      for _ in 1 .. n div 2:
        let
          v = pi.ps.pop()
          k = pi.ps.pop().getStr
        obj.add(k, v)
      pi.ps.add(obj)
    jarray     <- '[' * indent * JSON * *(',' * JSON) * ?(',' * S) * ']':
      var array = newJArray()
      let n = pi.ne.pop()
      for _ in 1 .. n:
        let e = pi.ps.pop()
        array.add(e)
      pi.ps.add(array)

  var pi = ParseInfo()
  pi.ne = newSeq[int]()
  pi.ps = newSeq[JsonNode]()

  doAssert p.match(s, pi).ok
  doAssert pi.ne.len == 1
  doAssert pi.ps.len == 1

  result = pi.ps[0]

proc isValidIdentifier(s: string): bool =
  let p = peg id:
    id <- Alpha * +('_' | Alnum)
  p.match(s).ok

proc indent(s: var string, i: int) =
  s.add(spaces(i))

proc newIndent(curr, indent: int, ml: bool): int =
  if ml: return curr + indent
  else: return indent

proc nl(s: var string, ml: bool) =
  s.add(if ml: "\n" else: " ")

proc escapeJsonSingle*(s: string; result: var string) =
  ## Converts a string `s` to its JSON representation with single quotes.
  ## Appends to ``result``.
  result.add("\'")
  escapeJsonUnquoted(s, result)
  result.add("\'")

proc toPretty6(result: var string, node: JsonNode, indent = 2, ml = true,
              lstArr = false, currIndent = 0) =
  case node.kind
  of JObject:
    if lstArr: result.indent(currIndent) # Indentation
    if node.fields.len > 0:
      result.add("{")
      result.nl(ml) # New line
      var i = 0
      for key, val in pairs(node.fields):
        if i > 0:
          result.add(",")
          result.nl(ml) # New Line
        inc i
        # Need to indent more than {
        result.indent(newIndent(currIndent, indent, ml))
        if isValidIdentifier(key):
          escapeJsonUnquoted(key, result)
        else:
          escapeJsonSingle(key, result)
        result.add(": ")
        toPretty6(result, val, indent, ml, false,
                 newIndent(currIndent, indent, ml))
      result.nl(ml)
      result.indent(currIndent) # indent the same as {
      result.add("}")
    else:
      result.add("{}")
  of JString:
    if lstArr: result.indent(currIndent)
    escapeJsonSingle(node.str, result)
  of JInt:
    if lstArr: result.indent(currIndent)
    when defined(js): result.add($node.num)
    else: result.addInt(node.num)
  of JFloat:
    if lstArr: result.indent(currIndent)
    # Fixme: implement new system.add ops for the JS target
    when defined(js): result.add($node.fnum)
    else: result.addFloat(node.fnum)
  of JBool:
    if lstArr: result.indent(currIndent)
    result.add(if node.bval: "true" else: "false")
  of JArray:
    if lstArr: result.indent(currIndent)
    if len(node.elems) != 0:
      result.add("[")
      result.nl(ml)
      for i in 0..len(node.elems)-1:
        if i > 0:
          result.add(",")
          result.nl(ml) # New Line
        toPretty6(result, node.elems[i], indent, ml,
            true, newIndent(currIndent, indent, ml))
      result.nl(ml)
      result.indent(currIndent)
      result.add("]")
    else: result.add("[]")
  of JNull:
    if lstArr: result.indent(currIndent)
    result.add("null")

proc pretty6*(node: JsonNode, indent = 2): string =
  toPretty6(result, node, indent)

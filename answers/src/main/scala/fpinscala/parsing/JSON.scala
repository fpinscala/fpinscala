package fpinscala.parsing

import language.higherKinds

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

object JSON:
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] =
    import P.*

    def str(s: String) = string(s).token

    def array = surround(str("["), str("]"))(
      value.sep(str(",")).map(vs => JArray(vs.toIndexedSeq))).scope("array")

    def obj = surround(str("{"), str("}"))(
      keyval.sep(str(",")).map(kvs => JObject(kvs.toMap))).scope("object")

    def keyval = escapedQuoted ** (str(":") *> value)

    def lit = (
      str("null").as(JNull) |
      double.map(JNumber(_)) |
      escapedQuoted.map(JString(_)) |
      str("true").as(JBool(true)) |
      str("false").as(JBool(false))
    ).scope("literal")

    def value: Parser[JSON] = lit | obj | array

    (whitespace *> (obj | array)).root

/**
 * JSON parsing example.
 */
@main def jsonExample =
  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""

  def printResult[E](e: Either[E,JSON]) =
    e.fold(println, println)

  val parser = JSON.jsonParser(Reference)
  printResult(parser.run(jsonTxt))
  println("--")
  printResult(parser.run(malformedJson1))
  println("--")
  printResult(parser.run(malformedJson2))

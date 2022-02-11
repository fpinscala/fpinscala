package fpinscala.exercises.parsing

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Gen.`**`
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.parsing.JSON.*
import fpinscala.exercises.parsing.{JSON, Parsers}

// Exercise 9.9
class JSONSuite extends PropSuite:
  private val parser = JSON.jsonParser(UnitTestParser)

  test("JSON.JNull")(Gen.unit(())) { _ =>
    assertEquals(parser.run("""{ "key": null }"""), Right(JObject(Map("key" -> JNull))))
    assertEquals(parser.run("""[ null ]"""), Right(JArray(IndexedSeq(JNull))))
    assertEquals(
      parser.run("""[ null, null, null ]"""),
      Right(JArray(IndexedSeq(JNull, JNull, JNull)))
    )
  }

  test("JSON.JNumber")(Gen.double ** Gen.double ** Gen.double) { case d1 ** d2 ** d3 =>
    assertEquals(parser.run(s"""{ "key": $d1 }"""), Right(JObject(Map("key" -> JNumber(d1)))))
    assertEquals(parser.run(s"""[ $d1 ]"""), Right(JArray(IndexedSeq(JNumber(d1)))))
    assertEquals(
      parser.run(s"""[ $d1, $d2, $d3 ]"""),
      Right(JArray(IndexedSeq(JNumber(d1), JNumber(d2), JNumber(d3))))
    )
  }

  test("JSON.JString")(genString ** genString ** genString) { case s1 ** s2 ** s3 =>
    assertEquals(parser.run(s"""{ "key": "$s1" }"""), Right(JObject(Map("key" -> JString(s1)))))
    assertEquals(parser.run(s"""[ "$s1" ]"""), Right(JArray(IndexedSeq(JString(s1)))))
    assertEquals(
      parser.run(s"""[ "$s1", "$s2", "$s3" ]"""),
      Right(JArray(IndexedSeq(JString(s1), JString(s2), JString(s3))))
    )
  }

  test("JSON.JBool")(Gen.boolean ** Gen.boolean ** Gen.boolean) { case b1 ** b2 ** b3 =>
    assertEquals(parser.run(s"""{ "key": $b1 }"""), Right(JObject(Map("key" -> JBool(b1)))))
    assertEquals(parser.run(s"""[ $b1 ]"""), Right(JArray(IndexedSeq(JBool(b1)))))
    assertEquals(
      parser.run(s"""[ $b1, $b2, $b3 ]"""),
      Right(JArray(IndexedSeq(JBool(b1), JBool(b2), JBool(b3))))
    )
  }

  test("JSON.JArray")(Gen.double ** genString ** Gen.boolean) { case d ** s ** b =>
    assertEquals(parser.run("[ ]"), Right(JArray(IndexedSeq.empty[JSON])))
    assertEquals(
      parser.run(s"""[ null, $d, "$s", $b ]"""),
      Right(JArray(IndexedSeq(JNull, JNumber(d), JString(s), JBool(b))))
    )
    assertEquals(
      parser.run(s"""[ null, [ $d, [ "$s", $b ] ] ]"""),
      Right(
        JArray(
          IndexedSeq(
            JNull,
            JArray(IndexedSeq(JNumber(d), JArray(IndexedSeq(JString(s), JBool(b)))))
          )
        )
      )
    )
  }

  private val jObjectJson1 = """
    {
      "Company name" : "Microsoft Corporation",
      "Ticker"  : "MSFT",
      "Active"  : true,
      "Price"   : 30.66,
      "Shares outstanding" : 8.38e9,
      "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
    }
    """

  test("JSON.JObject, 1")(Gen.unit(())) { _ =>
    assertEquals(
      parser.run(jObjectJson1),
      Right(
        JObject(
          Map(
            "Company name" -> JString("Microsoft Corporation"),
            "Ticker" -> JString("MSFT"),
            "Active" -> JBool(true),
            "Price" -> JNumber(30.66),
            "Shares outstanding" -> JNumber(8.38e9),
            "Related companies" -> JArray(
              IndexedSeq(JString("HPQ"), JString("IBM"), JString("YHOO"), JString("DELL"), JString("GOOG"))
            )
          )
        )
      )
    )
  }

  private val jObjectJson2 = """
    {
      "Book"   : "Functional Programming in Scala, Second Edition",
      "Active" : true,
      "Pages"  : 322,
      "Parts" : {
        "Part 1" : {
          "Title" : "Introduction to functional programming",
          "Content" : [
            {
              "Chapter 1": {
                "Title" : "What is functional programming?",
                "Content" : [
                  "1.1 The benefits of FP: a simple example",
                  [
                    "1.1.1 A program with side effects",
                    "1.1.2 A functional solution: removing the side effects"
                  ]
                ]
              }
            }
          ]
        }
      }
    }
    """

  test("JSON.JObject, 2")(Gen.unit(())) { _ =>
    assertEquals(
      parser.run(jObjectJson2),
      Right(
        JObject(
          Map(
            "Book" -> JString("Functional Programming in Scala, Second Edition"),
            "Active" -> JBool(true),
            "Pages" -> JNumber(322),
            "Parts" -> JObject(
              Map(
                "Part 1" -> JObject(
                  Map(
                    "Title" -> JString("Introduction to functional programming"),
                    "Content" -> JArray(
                      IndexedSeq(
                        JObject(
                          Map(
                            "Chapter 1" -> JObject(
                              Map(
                                "Title" -> JString("What is functional programming?"),
                                "Content" -> JArray(
                                  IndexedSeq(
                                    JString("1.1 The benefits of FP: a simple example"),
                                    JArray(
                                      IndexedSeq(
                                        JString("1.1.1 A program with side effects"),
                                        JString("1.1.2 A functional solution: removing the side effects")
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  private val genMalformedJSONs: Gen[String] =
    IndexedSeq(
      """
    "Company name"
    """,
      """
    "Company name" : "Microsoft Corporation"
    """,
      """
    {
      "Company name" : "Microsoft Corporation"
    """,
      """
      "Company name" : "Microsoft Corporation"
    }
    """,
      """
    {
      "Company name" ; "Microsoft Corporation"
    }
    """,
      """
    [ "HPQ" "IBM" ]
    """,
      """
    [
      [ "HPQ", "IBM",
      "YHOO", "DELL" ++
      "GOOG"
      ]
    ]
    """
    ).map(Gen.unit).reduce(Gen.union)

  test("malformed JSONs")(genMalformedJSONs) { json =>
    assert(parser.run(json).isLeft)
  }

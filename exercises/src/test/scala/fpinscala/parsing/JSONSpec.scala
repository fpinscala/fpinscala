package fpinscala.parsing

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Arbitrary

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class JSONSpec extends FlatSpec with PropertyChecks with ParserTest[TestParserTypes.Parser] {

  override val P = TestParser
  import TestParserTypes._
  import TestParser._

//  override val P = ParserImpl
//  import ParserTypes._
//  import ParserImpl._

  val json: Parser[JSON] = JSON.jsonParser(P)
  private def parse(jsonTxt: String): Either[ParseError,JSON] =
    P.run(json)(jsonTxt)

  import JSON._

  private implicit def arbJSON: Arbitrary[JSON] = {
    import org.scalacheck.Arbitrary._
    import org.scalacheck.Gen
    val MaxDepth = 5 // to prevent StackOverflows
    val genKey = limitedStringGen(1, 10)
    val genJNull: Gen[JSON] = Gen.const(JNull)
    val genJNumber: Gen[JSON] = arbDouble.arbitrary map(JNumber(_))
    val genJString: Gen[JSON] = limitedStringGen(0, 10) map(JString(_))
    val genJBool: Gen[JSON] = arbBool.arbitrary map(JBool(_))
    val genNonRoot = Gen.oneOf(genJNull, genJNumber, genJString, genJBool)
    def genJArray(depth: Int): Gen[JSON] = for {
      size <- Gen.chooseNum(0, 10)
      seq <- Gen.containerOfN[IndexedSeq,JSON](size, genJSON(depth + 1))
    } yield JArray(seq)
    def genJObject(depth: Int): Gen[JSON] = for {
      size <- Gen.chooseNum(0, 10)
      map <- Gen.mapOfN[String,JSON](size, Gen.zip(genKey, genJSON(depth + 1)))
    } yield JObject(map)
    def genJSON(depth: Int) = if (depth < MaxDepth) Gen.oneOf(genNonRoot, genRoot(depth)) else genNonRoot
    def genRoot(depth: Int) = Gen.oneOf(genJArray(depth), genJObject(depth))
    Arbitrary(genRoot(0))
  }

  private def quote(str: String) = s""""$str""""

  private def toJson(json: JSON): String = json match {
    case JNull => "null"
    case JNumber(d) => d.toString
    case JString(s) => quote(s)
    case JBool(b) => b.toString
    case JArray(seq) => seq map(toJson(_)) mkString("[", ", ", "]")
    case JObject(jMap) => jMap map {case (k,v) => s"${quote(k)} : ${toJson(v)}"} mkString("{\n", ",\n", "\n}")
  }

  behavior of "9.9 jsonParser"

  it should "parse the different JSON types" in {
    val jsonTypeTests = Table(
      ("jsonTxt", "parse(jsonTxt)"),
      (null, JNull),
      (42, JNumber(42.0d)),
      (quote("foo"), JString("foo")),
      (true, JBool(true)),
      ("""[null, 42, "foo", true, [ "bar" ], { "x" : "baz"}]""",
          JArray(IndexedSeq(JNull, JNumber(42.0d), JString("foo"), JBool(true),
              JArray(IndexedSeq(JString("bar"))),
              JObject(Map("x" -> JString("baz")))))),
      ("""{
            "a" : null,
            "b" : 42,
            "c" : "foo",
            "d" : true,
            "e" : [ "bar" ],
            "f" : { "x" : "baz" }
          }""",
          JObject(Map("a" -> JNull, "b" -> JNumber(42.0d), "c" -> JString("foo"),
              "d" -> JBool(true), "e" -> JArray(IndexedSeq(JString("bar"))),
              "f" -> JObject(Map("x" -> JString("baz"))))))
    )
    forAll(jsonTypeTests) { (basicType, jsonValue) =>
      val jsonTxt = s"""{ "key" : $basicType }"""
      val expected = Right(JObject(Map("key" -> jsonValue)))
      assert(parse(jsonTxt) == expected)
    }
  }

  it should "reject invalid json strings" in {
    val invalidJsonTests = Table(
      ("invalidJson"),
      ("""true"""),
      ("""{ true }"""),
      (""" "a" : true }"""),
      ("""{ "a" : true """),
      ("""{ "a" , true }"""),
      ("""{ "a" : true] }"""),
      ("""{ "a" : [true }"""),
      ("""{ "a" :  "b" : true } }"""),
      ("""{ "a" : { "b" : true } """)
    )
    forAll(invalidJsonTests) { invalidJson =>
      assert(parse(invalidJson).isLeft)
    }
  }

  it should "parse arbitrary JSON" in {
    forAll("json") { json: JSON =>
//      println(json)
//      println(toJson(json))
      assert(parse(toJson(json)) == Right(json))
    }
  }
}

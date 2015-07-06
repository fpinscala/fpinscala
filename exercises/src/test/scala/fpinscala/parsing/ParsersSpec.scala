package fpinscala.parsing

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ParsersSpec extends FlatSpec with PropertyChecks with ParserTest[TestParserTypes.Parser] {

  override val P = TestParser
  import TestParserTypes._
  import TestParser._

  behavior of "9.1.1 map2ViaProduct"

  it should "work" in {
    import Exercises.map2ViaProduct
    val parser: Parser[String] = map2ViaProduct("abra", "cadabra")(_ + _)
    assert(TestParser.run(parser)("abracadabra") == Right("abracadabra"),
        """run(parser)("abracadabra")""")
    assert(TestParser.run(parser)("abracadabrax") == Right("abracadabra"),
        """run(parser)("abracadabrax")""")

    assert(run(parser)("abra cadabra") ==
      parseError(Location("abra cadabra", 4), s"""string: " cadabra" does not start with "cadabra""""),
        """run(parser)("abra cadabra")""")
  }

  behavior of "9.1.2 many1"

  it should "work" in {
    val parser: Parser[List[Char]] = many1(char('a'))
    assert(run(parser)("aaa") == Right(List.fill(3)('a')), """run(parser)("aaa")""")

    assert(run(parser)("baaa") ==
      parseError(Location("baaa", 0), s"""string: "baaa" does not start with "a""""),
        """run(parser)("baaa")""")
  }

  behavior of "9.2 product law"

  it should "hold" in {
    forAll(limitedStringGen(1, 10) label "s") { s: String =>
      val p: Parser[String] = string(s)
      equal(product(p,p), p.map((a:String) => (a,a)))
    }
  }

  behavior of "9.3 many"

  it should "work" in {
    val parser: Parser[List[Char]] = many(char('a'))
    assert(run(parser)("aaa") == Right(List.fill(3)('a')), """run(parser)("aaa")""")
    assert(run(parser)("baaa") == Right(List()), """run(parser)("baaa")""")
    assert(run(parser)("") == Right(List()), """run(parser)("")""")
  }

  behavior of "9.4 listOfN"

  it should "work" in {
    // seems to be a bug in the book: return type of listOfN is Parser[List[A]], not Parser[A]
    assert(run(listOfN(3, "ab" | "cad"))("ababcad") == Right(List("ab", "ab", "cad")),
        """150: run(listOfN(3, "ab" | "cad"))("ababcad")""")
    assert(run(listOfN(3, "ab" | "cad"))("cadabab") == Right(List("cad", "ab", "ab")),
        """150: run(listOfN(3, "ab" | "cad"))("cadabab")""")
    assert(run(listOfN(3, "ab" | "cad"))("ababab") == Right(List("ab", "ab", "ab")),
        """150: run(listOfN(3, "ab" | "cad"))("ababab")""")
    assert(run(listOfN(3, "ab" | "cad"))("abababx") == Right(List("ab", "ab", "ab")),
        """run(listOfN(3, "ab" | "cad"))("abababx")""")

    assert(run(listOfN(3, "ab" | "cad"))("ababaxb") ==
      parseError(Location("ababaxb", 4), s"""string: "axb" does not start with "cad""""),
        """run(listOfN(3, "ab" | "cad"))("ababaxb")""")
  }

  behavior of "9.6 csListOfN"

  import TestParser.Exercises.csListOfN

  it should "succeed for \"4aaaa\"" in {
    val parser: Parser[List[Char]] = csListOfN(char('a'))
    assert(run(parser)("4aaaa") == Right(List.fill(4)('a')))
  }

  it should "fail for \"4aaa\"" in {
    assert(run(csListOfN(char('a')))("4aaa") ==
           parseError(Location("4aaa", 4), s"""string: "" does not start with "a""""))
  }

  it should "obey the law on page 157"  in {
    val parser: Parser[Char] = char('a')
    val strSizeGen = Gen.chooseNum(0, 10) label "n"
    forAll(strSizeGen) { n: Int =>
      assert(run(csListOfN(parser))(n + ("a" * n)).right.get.size == n)
    }
  }

  behavior of "9.7.1 product"

  it should "work" in {
    val as = many1(char('a')) map((_:List[Char]).mkString)
    val bs = many1(char('b')) map((_:List[Char]).mkString)
    val abProduct = product(as, bs)
    assert(run(abProduct)("ab") == Right(("a","b")), """run(abProduct)("ab")""")
    assert(run(abProduct)("aabb") == Right(("aa","bb")), """run(abProduct)("aabb")""")

    assert(run(abProduct)("") ==
      parseError(Location("", 0), s"""string: "" does not start with "a""""), """run(abProduct)("")""")
    assert(run(abProduct)("xab") ==
      parseError(Location("xab", 0), s"""string: "xab" does not start with "a""""), """run(abProduct)("xab")""")
    assert(run(abProduct)("axb") ==
      parseError(Location("axb", 1), s"""string: "xb" does not start with "b""""), """run(abProduct)("axb")""")
  }

  behavior of "9.7.2 map2"

  it should "work" in {
    val parser: Parser[String] = map2("abra", "cadabra")(_ + _)
    assert(run(parser)("abracadabra") == Right("abracadabra"),
        """run(parser)("abracadabra")""")
    assert(run(parser)("abracadabrax") == Right("abracadabra"),
        """run(parser)("abracadabrax")""")

    assert(run(parser)("abra cadabra") ==
      parseError(Location("abra cadabra", 4), s"""string: " cadabra" does not start with "cadabra""""),
        """run(parser)("abra cadabra")""")
  }

  behavior of "9.8 map"

  private def equal[A](p1: Parser[A], p2: Parser[A]) = {
    forAll("in") { in: String => assert(run(p1)(in) == run(p2)(in)) }
  }

  it should "preserve structure, p.150" in {
    forAll("p") { p: Parser[String] => equal(p, p.map(identity[String])) }
//    forAll("p", "in") {(p: Parser[String], in: String) =>
//      assert(run(p)(in) == run(p.map(identity[String]))(in))
//    }
  }

  behavior of "Laws"

  it should "hold for 149: singleCharLaw" in {
    def singleCharLaw(c: Char) = run(char(c))(c.toString) == Right(c) // 149
    forAll("c") { c: Char => assert(singleCharLaw(c), s"singleCharLaw($c)") }
  }

  it should "hold for 149: singleStringLaw" in {
    def singleStringLaw(s: String) = run(string(s))(s) == Right(s) // 149
    forAll("s") { s: String => assert(singleStringLaw(s), s"""singleStringLaw("$s")""") }
  }

  it should "hold for 153: succeedLaw" in {
    def succeedLaw[A](a: A)(s: String) = run(succeed(a))(s) == Right(a) // 153
    forAll("n", "s") { (n: Int, s: String) =>
      assert(succeedLaw(n)(s), s"""succeedLaw("$n", "$s")""") }
  }

  it should "hold for 154: numALaw" in {
    forAll("p") {(p: Parser[String]) =>
      equal(p.many.map((_: List[String]).size), slice(p.many).map((_: String).size))
    }
  }

  behavior of "Facts"

  it should "all be true" in {
    val numA: Parser[Int] = char('a').many.map((_: List[Char]).size) // 152
    val numA1 = char('a').many.slice.map((_: String).size) // 154

    val facts: Map[String,Boolean] = Map(
      """149: run(or(string("abra"),string("cadabra")))("abra") == Right("abra")""" ->
        (run(or(string("abra"), string("cadabra")))("abra") == Right("abra")), // 149
      """149: run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")""" ->
        (run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")),

      // seems to be a bug in the book: return type of listOfN is Parser[List[A]], not Parser[A]
      """150: run(listOfN(3, "ab" | "cad"))("ababcad") == Right(List("ab", "ab", "cad"))""" ->
        (run(listOfN(3, "ab" | "cad"))("ababcad") == Right(List("ab", "ab", "cad"))), // 150
      """150: run(listOfN(3, "ab" | "cad"))("cadabab") == Right(List("cad", "ab", "ab"))""" ->
        (run(listOfN(3, "ab" | "cad"))("cadabab") == Right(List("cad", "ab", "ab"))),
      """150: run(listOfN(3, "ab" | "cad"))("ababab") == Right(List("ab", "ab", "ab"))""" ->
        (run(listOfN(3, "ab" | "cad"))("ababab") == Right(List("ab", "ab", "ab"))),

      """154: run(numA)("aaa") == Right(3)""" ->
        (run(numA)("aaa") == Right(3)),
      """154: run(numA)("b") == Right(0)""" ->
        (run(numA)("b") == Right(0)),
      """154: run(numA1)("aaa") == Right(3)""" ->
        (run(numA1)("aaa") == Right(3)),
      """154: run(numA1)("b") == Right(0)""" ->
        (run(numA1)("b") == Right(0)),
      """154: run(slice((char('a') | char('b')).many))("aaba") == Right("aaba")""" ->
        (run(slice((char('a') | char('b')).many))("aaba") == Right("aaba")) // 154
    )

    facts.foreach {case (k, v) => assert(v, k)}
  }
}

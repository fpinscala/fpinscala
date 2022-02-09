package fpinscala.exercises.parsing

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.parsing.Parsers

class ParsersSuite extends PropSuite:
  import UnitTestParser.*

  /*
  test("Parsers.char")(genChar) { c =>
    assertEquals(char(c).run(c.toString), Right(c))
    val anotherChar = (c + 1).toChar
    assert(char(c).run(anotherChar.toString).isLeft)
  }

  test("Parsers.string")(genString) { s =>
    assertEquals(string(s).run(s), Right(s))

    val nonEmptyS = s"a$s"
    val anotherString = s"b$s"
    assert(string(nonEmptyS).run(anotherString).isLeft)
  }

  test("Parsers.or")(Gen.unit(())) { _ =>
    val orParser = string("abra").or(string("cadabra"))
    assertEquals(orParser.run("abra"), Right("abra"))
    assertEquals(orParser.run("cadabra"), Right("cadabra"))
    assert(orParser.run("error").isLeft)

    val or1Parser = string("abra") | string("cadabra")
    assertEquals(or1Parser.run("abra"), Right("abra"))
    assertEquals(or1Parser.run("cadabra"), Right("cadabra"))
    assert(or1Parser.run("error").isLeft)
  }

  test("Parsers.listOfN")(Gen.unit(())) { _ =>
    val p = (string("ab") | string("cad")).listOfN(3)
    assertEquals(p.run("ababcad"), Right(List("ab", "ab", "cad")))
    assertEquals(p.run("cadabab"), Right(List("cad", "ab", "ab")))
    assertEquals(p.run("ababab"), Right(List("ab", "ab", "ab")))
    assertEquals(p.run("cadcadcad"), Right(List("cad", "cad", "cad")))
    assertEquals(p.run("ababcad_any_another_string"), Right(List("ab", "ab", "cad")))
    assert(p.run("abab").isLeft)
  }

  test("Parsers.many")(Gen.unit(())) { _ =>
    val numA: Parser[Int] = char('a').many.map(_.size)
    assertEquals(numA.run("aaa"), Right(3))
    assertEquals(numA.run("a"), Right(1))
    assertEquals(numA.run("b"), Right(0))
  }

  test("Parsers.succeed")(genString) { s =>
    assertEquals(succeed("succeed").run(s), Right("succeed"))
  }

  test("Parsers.slice")(Gen.unit(())) { _ =>
    assertEquals((char('a') | char('b')).many.slice.run("aaba"), Right("aaba"))
  }

  test("Exercise 9.1, map2")(Gen.unit(())) { _ =>
    val parserA = char('a')
    val parserB = char('b')
    val parserC = parserA.map2(parserB)((a, b) => s"$a$b")
    assertEquals(parserC.run("ab"), Right("ab"))
    assert(parserC.run("ba").isLeft)
    assert(parserC.run("aa").isLeft)
    assert(parserC.run("a").isLeft)
    assert(parserC.run("b").isLeft)
    assert(parserC.run("c").isLeft)
  }

  test("Exercise 9.1, many1")(Gen.unit(())) { _ =>
    val numA: Parser[Int] = char('a').many1.map(_.size)
    assertEquals(numA.run("aaa"), Right(3))
    assertEquals(numA.run("a"), Right(1))
    assert(numA.run("b").isLeft)

    val zeroOrMoreAFollowedByOneOrMoreB = char('a').many.slice.map(_.length) ** char('b').many1.slice.map(_.length)
    assertEquals(zeroOrMoreAFollowedByOneOrMoreB.run("bbb"), Right((0, 3)))
    assertEquals(zeroOrMoreAFollowedByOneOrMoreB.run("abbb"), Right((1, 3)))
    assertEquals(zeroOrMoreAFollowedByOneOrMoreB.run("aaabbb"), Right((3, 3)))
    assertEquals(zeroOrMoreAFollowedByOneOrMoreB.run("b"), Right((0, 1)))
    assertEquals(zeroOrMoreAFollowedByOneOrMoreB.run("ab"), Right((1, 1)))
    assertEquals(zeroOrMoreAFollowedByOneOrMoreB.run("aaab"), Right((3, 1)))
    assert(zeroOrMoreAFollowedByOneOrMoreB.run("").isLeft)
    assert(zeroOrMoreAFollowedByOneOrMoreB.run("a").isLeft)
    assert(zeroOrMoreAFollowedByOneOrMoreB.run("aaa").isLeft)
    assertEquals(zeroOrMoreAFollowedByOneOrMoreB.run("aaabbbab"), Right((3, 3)))
    assert(zeroOrMoreAFollowedByOneOrMoreB.run("c").isLeft)
  }

  private val lawsParser: Gen[(Parser[Char], Parser[Char], Parser[Char], String, String)] =
    for
      a <- genChar
      b <- genChar
      c <- genChar
    yield (char(a), char(b), char(c), s"$a$b$c", s"$a$a$a")

  test("Exercise 9.2")(lawsParser) { case (a, b, c, success, failure) =>
    val parserL = ((a ** b) ** c).map(unbiasL)
    val parserR = (a ** (b ** c)).map(unbiasR)
    assertEquals(parserL.run(success), parserR.run(success))
    assertEquals(parserL.run(failure), parserR.run(failure))

    val f: Char => String = c => s"$c$c"
    val g: Char => String = c => s"$c$c$c"
    val productOfMapParser = a.map(f) ** b.map(g)
    val mapOfProductParser = (a ** b).map((a, b) => (f(a), g(b)))
    assertEquals(productOfMapParser.run(success), mapOfProductParser.run(success))
    assertEquals(productOfMapParser.run(failure), mapOfProductParser.run(failure))
  }

  private val examples = new Examples(UnitTestParser)
  import examples.*

  private val genNonNegativeIntExamples: Gen[(Int, Char, String)] =
    for
      posInt <- Gen.int.map(_.abs)
      char <- genChar
      str <- genString
    yield (posInt, char, str)

  test("Exercise 9.6, nonNegativeInt")(genNonNegativeIntExamples) { case (posInt, char, str) =>
    assertEquals(nonNegativeInt.run(posInt.toString), Right(posInt))
    assert(nonNegativeInt.run(s"-$posInt").isLeft)
    assert(nonNegativeInt.run(char.toString).isLeft)
    assert(nonNegativeInt.run(str).isLeft)
  }

  test("Exercise 9.6, nConsecutiveAs")(Gen.unit(())) { _ =>
    assertEquals(nConsecutiveAs.run("0"), Right(0))
    assertEquals(nConsecutiveAs.run("1a"), Right(1))
    assertEquals(nConsecutiveAs.run("2aa"), Right(2))
    assertEquals(nConsecutiveAs.run("4aaaa"), Right(4))
    assertEquals(nConsecutiveAs.run("0aaaa"), Right(0))
    assertEquals(nConsecutiveAs.run("2aaaa"), Right(2))
    assert(nConsecutiveAs.run("4bcde").isLeft)
    assert(nConsecutiveAs.run("4baaa").isLeft)
    assert(nConsecutiveAs.run("aaaa").isLeft)
    assert(nConsecutiveAs.run("-4aaaa").isLeft)
  }

  private def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p(0)(0), p(0)(1), p(1))
  private def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p(0), p(1)(0), p(1)(1))
  */

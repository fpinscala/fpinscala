package fpinscala.parsing

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ParserImplSpec extends FlatSpec with PropertyChecks with ParserTest[ParserTypes.Parser] {

  override val P = ParserImpl
  import ParserTypes._
  import ParserImpl._

  private implicit def toLocation(s: String) = Location(s)

  private def assertFailure(result: Result[_], isCommitted: Boolean, description: String) = result match {
    case Failure(_, isCommitted) =>
    case _ => fail(s"$description: expected to be Failure with isCommitted = $isCommitted, but was $result")
  }

  behavior of "9.13.1 string"

  it should "work" in {
    forAll(limitedStringGen(1, 10) label "s") { s: String =>
      val (p,ps) = (string(s), s"""string("$s")""")
      assert(p(s) == Success(s, s.length), s"""$ps("$s")""")
      assert(p(s + "_") == Success(s, s.length), s"""$ps("${s}_")""")
      assert(p(Location("_" + s, 1)) == Success(s, s.length), s"""$ps(Location("_$s",1))""")

      assertFailure(p("_" + s), false, s"""$ps("_$s")""")
    }
    assertFailure(string("aaa")("ab"), true, """string("aaa")("ab") [commit if heads match]""")
  }

  behavior of "9.13.2 regex"

  it should "work" in {
    forAll(limitedStringGen(1, 10) label "s") { s: String =>
      val (p,ps) = (regex(s.r), s"""regex("$s")""")
      assert(p(s) == Success(s, s.length), s"""$ps("$s")""")
      assert(p(s + "_") == Success(s, s.length), s"""$ps("${s}_")""")
      assert(regex(s".$s".r)("_" + s) == Success("_" + s, s.length + 1), s"""regex(".$s".r)("_$s")""")
      assert(p(Location("_" + s, 1)) == Success(s, s.length), s"""$ps(Location("_$s",1))""")

      assertFailure(p("_" + s), false, s"""$ps("_$s")""")
    }
  }

  behavior of "9.13.3 succeed"

  it should "work" in {
    forAll("s") { s: String =>
      val (p,ps) = (succeed(s), s"""succeed("$s")""")
      assert(p("x") == Success(s, 0), s"""$ps("x")""")
    }
    forAll("i") { i: Int =>
      val (p,ps) = (succeed(i), s"""succeed($i)""")
      assert(p("x") == Success(i, 0), s"""$ps("x")""")
    }
  }

  behavior of "9.13.4 slice"

  it should "work" in {
    forAll(limitedStringGen(1, 10) label "s") { s: String =>
      val (p,ps) = (slice(string(s)), s"""slice(string("$s"))""")
      assert(p(s) == Success(s, s.length), s"""$ps("$s")""")
      assert(p(s + "_") == Success(s, s.length), s"""$ps("${s}_")""")
      assert(p(Location("_" + s, 1)) == Success(s, s.length), s"""$ps(Location("_$s",1))""")

      assertFailure(p("_" + s), false, s"""$ps("_$s")""")
    }
    assertFailure(slice(string("aaa"))("ab"), true, """slice(string("aaa"))("ab") [commit if heads match]""")
  }

}
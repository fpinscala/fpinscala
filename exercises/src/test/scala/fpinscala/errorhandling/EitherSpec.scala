package fpinscala.errorhandling

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.oneOf
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class EitherSpec extends FlatSpec with PropertyChecks {

  // copied from org.scalacheck.Arbitrary
  implicit def arbEither[T, U](implicit at: Arbitrary[T], au: Arbitrary[U]): Arbitrary[Either[T, U]] =
    Arbitrary(oneOf(arbitrary[T].map(Left(_)), arbitrary[U].map(Right(_))))

  behavior of "4.6.1 map"

  it should "work" in {
    def testMap[A](e: Either[_, A], expected: Either[_, String]) =
      assertResult(expected)(e.map(_.toString + "x"))

    val tests = Table(
      ("Either[_,A]", "map(...)"),
      (Left("oops"), Left("oops")),
      (Right(1), Right("1x")),
      (Right("a"), Right("ax")),
      (Right(List()), Right("List()x")),
      (Right(List(1, 2, 3)), Right("List(1, 2, 3)x")))
    forAll(tests)(testMap)
  }

  it should "for all e: Either[_,Int] ==> e.map(_.toString).map(_.toInt) == e" in {
    forAll("Either[_,Int]") { e: Either[String,Int] =>
      assertResult(e)(e.map(_.toString).map(_.toInt))
    }
  }

  behavior of "4.6.2 flatMap"

  it should "work" in {
    def testFlatMap[A](e: Either[_, A], expected: Either[_, String]) =
      assertResult(expected)(e.flatMap(r => Right(r.toString + "x")))

    val tests = Table(
      ("Either[_,A]", "flatMap(...)"),
      (Left("oops"), Left("oops")),
      (Right(1), Right("1x")),
      (Right("a"), Right("ax")),
      (Right(List()), Right("List()x")),
      (Right(List(1, 2, 3)), Right("List(1, 2, 3)x")))
    forAll(tests)(testFlatMap)
  }

  it should "for all e: Either[_,Int] ==> e.flatMap(r => Right(r.toString).flatMap(r => Right(r.toInt)) == e" in {
    forAll("Either[_,Int]") { e: Either[String,Int] =>
      assertResult(e)(e.flatMap(r => Right(r.toString)).flatMap(r => Right(r.toInt)))
    }
  }

  behavior of "4.6.3 orElse"

  it should "work" in {
    def testOrElse(e: Either[String,Int], b: Either[String,Int], expected: Either[String,Int]) =
      assertResult(expected)(e.orElse(b))

    val tests = Table(
      ("Either[String,Int]", "b: Either[String,Int]", "orElse(b)"),
      (Left("oops"), Right(1), Right(1)),
      (Left("oops"), Left("whoa"), Left("whoa")),
      (Right(1), Right(2), Right(1)),
      (Right(1), Left("whoa"), Right(1)))
    forAll(tests)(testOrElse)
  }

  it should "for all e: Either[String,Int] ==> e.orElse(e) == e" in {
    forAll("Either[_,Int]") { e: Either[String,Int] =>
      assertResult(e)(e.orElse(e))
    }
  }

  behavior of "4.6.4 map2"

  it should "work" in {
    def testMap2(e: Either[String,Int], b: Either[String,Int], expected: Either[String,Int]) =
      assertResult(expected)(e.map2(b)(_ + _))

    val tests = Table(
      ("Either[String,Int]", "b: Either[String,Int]", "map2(b)"),
      (Left("oops"), Right(1), Left("oops")),
      (Left("oops"), Left("whoa"), Left("oops")),
      (Right(1), Left("whoa"), Left("whoa")),
      (Right(1), Right(2), Right(3)))
    forAll(tests)(testMap2)
  }

  it should "for all e: Either[String,Int] ==> e.map2(e)(_ + _) == e or Right(a+a)" in {
    forAll("Either[_,Int]") { e: Either[String,Int] =>
      val expected = e match {
        case left @ Left(_) => left
        case Right(a) => Right(a + a)
      }
      assertResult(expected)(e.map2(e)(_ + _))
    }
  }
}
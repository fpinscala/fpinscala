package fpinscala.errorhandling

import scala.{Option => _, Either => _, _}
import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.const
import org.scalacheck.Gen.frequency
import org.scalacheck.Gen.resize
import org.scalacheck.Gen.sized
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import Option._
import org.scalacheck.Gen

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class OptionSpec extends FlatSpec with PropertyChecks {

  implicit def arbOption[T](implicit a: Arbitrary[T]): Arbitrary[Option[T]] =
    Arbitrary(sized(n =>
      // When n is larger, make it less likely that we generate None,
      // but still do it some of the time. When n is zero, we always
      // generate None, since it's the smallest value.
      frequency(
        (n, resize(n / 2, arbitrary[T]).map(Some(_))),
        (1, const(None)))))

  behavior of "4.1.1 map"

  it should "work" in {
    def testMap(o: Option[_], expected: Option[String]) = assertResult(expected)(o.map(_.toString + "x"))

    val tests = Table(
      ("Option[_]", "map(...)"),
      (None, None),
      (Some(1), Some("1x")),
      (Some("a"), Some("ax")),
      (Some(List()), Some("List()x")),
      (Some(List(1, 2, 3)), Some("List(1, 2, 3)x")))
    forAll(tests)(testMap)
  }

  it should "for all o: Option[Int] ==> o.map(_.toString).map(_.toInt) == o" in {
    forAll("Option[Int]") { o: Option[Int] =>
      assertResult(o)(o.map(_.toString).map(_.toInt))
    }
  }

  behavior of "4.1.2 getOrElse"

  it should "work" in {
    def testGetOrElse[A,B>:A](o: Option[A], default: B, expected: B) =
      assertResult(expected)(o.getOrElse(default))

    val tests = Table(
      ("Option[_]", "default", "getOrElse(...)"),
      (None, "a", "a"),
      (Some(1), 2, 1),
      (Some("a"), "b", "a"))
    forAll(tests)(testGetOrElse)
  }

  behavior of "4.1.3 flatMap"

  it should "work" in {
    val someString = (i: Any) => Some(i.toString + "x")
    val  alwaysNone = (i: Any) => None
    def testFlatMap(o: Option[_], f: Any => Option[String], expected: Option[String]) =
      assertResult(expected)(o.flatMap(f))

    val tests = Table(
      ("Option[Int]", "f", "flatMap(...)"),
      (None, someString, None),
      (None, alwaysNone, None),
      (Some(1), someString, Some("1x")),
      (Some("a"), someString, Some("ax")),
      (Some(List()), someString, Some("List()x")),
      (Some(List(1, 2, 3)), someString, Some("List(1, 2, 3)x")))
    forAll(tests)(testFlatMap)
  }

  it should "for all o: Option[Int] ==> o.flatMap(x => Some(x.toString)).map(_.toInt) == o" in {
    forAll("Option[Int]") { o: Option[Int] =>
      assertResult(o)(o.flatMap(x => Some(x.toString)).map(_.toInt))
    }
  }

  behavior of "4.1.4 orElse"

  it should "work" in {
    def testOrElse[A,B>:A](o: Option[A], default: Option[B], expected: Option[B]) =
      assertResult(expected)(o.orElse(default))

    val tests = Table(
      ("Option[_]", "default", "orElse(default)"),
      (None, Some("a"), Some("a")),
      (Some(1), Some(2), Some(1)),
      (Some("a"), Some("b"), Some("a")))
    forAll(tests)(testOrElse)
  }

  it should "for all o: Option[Int] ==> o.orElse(o) == o" in {
    forAll("Option[Int]") { o: Option[Int] =>
      assertResult(o)(o.orElse(o))
    }
  }

  behavior of "4.1.5 filter"

  it should "work" in {
    def testFilter[A](o: Option[A], filterResult: Boolean, expected: Option[A]) =
      assertResult(expected)(o.filter(_ => filterResult))

    val tests = Table(
      ("Option[_]", "filterResult", "filter(...)"),
      (None, true, None),
      (Some(1), true, Some(1)),
      (Some(1), false, None),
      (Some("a"), true, Some("a")),
      (Some("a"), false, None))
    forAll(tests)(testFilter)
  }

  it should "for all o: Option[Int] ==> o.filter(true) == o" in {
    forAll("Option[Int]") { o: Option[Int] =>
      assertResult(o)(o.filter(_ => true))
    }
  }

  it should "for all o: Option[Int] ==> o.filter(false) == None" in {
    forAll("Option[Int]") { o: Option[Int] =>
      assertResult(None)(o.filter(_ => false))
    }
  }

  behavior of "4.2 variance"

  it should "be None for an empty Seq" in {
    assertResult(None)(variance(Seq.empty))
  }

  it should "be 0 for all sequences with same number" in {
    forAll (Gen.chooseNum(1, 10) :| "n", arbitrary[Int] :| "x") { (n: Int, x: Int) =>
      assertResult(Some(0))(variance(Seq.fill(n)(x)))
    }
  }

  behavior of "4.3 map2"

  it should "work" in {
    def testMap2(oa: Option[Int], ob: Option[String], expected: Option[Double]) =
      assertResult(expected)(map2(oa, ob)((a, b) => a + b.toDouble))

    val tests = Table(
      ("Option[Int]", "Option[String]", "map2(...)"),
      (None, None, None),
      (Some(1), None, None),
      (None, Some("2.3"), None),
      (Some(1), Some("2.3"), Some(3.3d)))
    forAll(tests)(testMap2)
  }

  behavior of "4.4 sequence"

  it should "work" in {
    def testSequence[A](oas: List[Option[A]], expected: Option[List[A]]) =
      assertResult(expected)(sequence(oas))

    val tests = Table(
      ("List[Option[_]]", "sequence(...)"),
      (Nil, Some(Nil)),
      (List(None), None),
      (List(Some(1)), Some(List(1))),
      (List(Some(1), Some(2)), Some(List(1,2))),
      (List(Some(1), None, Some(2)), None))
    forAll(tests)(testSequence)
  }

  behavior of "4.5 traverse"

  it should "work" in {
    def testTraverse(as: List[String], expected: Option[List[Int]]) =
      assertResult(expected)(traverse(as)(a => if (a.toInt % 2 == 0) Some(a.toInt) else None))

    val tests = Table(
      ("List[String]", "traverse(...)"),
      (Nil, Some(Nil)),
      (List("1"), None),
      (List("2", "3"), None),
      (List("2", "4"), Some(List(2, 4))))
    forAll(tests)(testTraverse)
  }

}
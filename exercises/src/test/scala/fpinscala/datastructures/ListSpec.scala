package fpinscala.datastructures

import scala.{List => SList}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import List._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ListSpec extends FlatSpec with PropertyChecks {

  def head[A](as: List[A]) = as match {
    case Nil => sys.error("head of empty list")
    case Cons(h, _) => h
  }

  private implicit def arbList[T](implicit ev: Arbitrary[Array[T]]): Arbitrary[List[T]] =
//    Arbitrary(arbitrary[Array[T]] map (List(_: _*)))
    Arbitrary(for {
      as <- arbitrary[Array[T]]
    } yield List(as: _*))

  private def arbListTuple[T](implicit ev: Arbitrary[Array[T]]): Arbitrary[(List[T],SList[T])] =
    Arbitrary(for {
      as <- arbitrary[Array[T]]
    } yield (List(as: _*), as.toList))

  private def toList[A](l: SList[A]) = List(l: _*)

  behavior of "3.2 tail"

  it should "throw a RuntimeException when passed an empty List" in {
    intercept[RuntimeException] {
      tail(Nil)
    }
  }

  it should "work" in {
    def testTail[A,B](as: List[A], expected: B) = assertResult(expected)(tail(as))

    val tests = Table(
      ("as", "tail(as)"),
      (List(0), Nil),
      (List(0, 1), List(1)),
      (List("a"), Nil),
      (List("a", "b"), List("b")))
    forAll(tests)(testTail)
  }

  it should "for all as: List[Int] ==> Cons(head(as), tail(as)) == as" in {
    forAll("as") { as: List[Int] =>
      whenever(as != Nil) {
        assertResult(Cons(head(as), tail(as)))(as)
      }
    }
  }

  it should "for all as: List[String] ==> Cons(head(as), tail(as)) == as" in {
    forAll("as") { as: List[String] =>
      whenever(as != Nil) {
        assertResult(Cons(head(as), tail(as)))(as)
      }
    }
  }

  it should "be equivalent to Scala List function" in {
    forAll(arbListTuple[Int].arbitrary) { case (l, sl) =>
      whenever(!sl.isEmpty) {
        assertResult(toList(sl.tail))(tail(l))
      }
    }
  }

  behavior of "3.3 setHead"

  it should "throw a RuntimeException when passed an empty List" in {
    intercept[RuntimeException] {
      setHead(Nil, 1)
    }
  }

  it should "work" in {
    def testSetHead[A](as: List[A], h: A, expected: List[A]) =
      assertResult(expected)(setHead(as, h))

    val tests = Table(
      ("as", "h", "setHead(as, h)"),
      (List(0), 1, List(1)),
      (List(0, 1), 2, List(2, 1)),
      (List("a"), "b", List("b")),
      (List("a", "b"), "c", List("c", "b")))
    forAll(tests)(testSetHead)
  }

  it should "for all as: List[Int] ==> setHead(as, head(as)) == as" in {
    forAll("as") { as: List[Int] =>
      whenever(as != Nil) {
        assertResult(setHead(as, head(as)))(as)
      }
    }
  }

  it should "for all as: List[String] ==> setHead(as, head(as)) == as" in {
    forAll("as") { as: List[String] =>
      whenever(as != Nil) {
        assertResult(setHead(as, head(as)))(as)
      }
    }
  }

  it should "for all as: (List[Int], h: Int) ==> setHead(as, h) == Cons(h, tail(as))" in {
    forAll("as", "h") { (as: List[Int], h: Int) =>
      whenever(as != Nil) {
        assertResult(setHead(as, h))(Cons(h, tail(as)))
      }
    }
  }

  it should "for all as: List[String], h: String ==> setHead(as, h) == Cons(h, tail(as))" in {
    forAll("as", "h") { (as: List[String], h: String) =>
      whenever(as != Nil) {
        assertResult(setHead(as, h))(Cons(h, tail(as)))
      }
    }
  }

  behavior of "3.4 drop"

  it should "work" in {
    def testDrop[A](as: List[A], n: Int, expected: List[A]) =
      assertResult(expected)(drop(as, n))
    val l123 = List(1,2,3)
    val labc = List("a","b","c")

    val tests = Table(
      ("l", "n", "drop(l, n)"),
      (Nil, 1, Nil),
      (Nil, 2, Nil),

      (l123, Int.MinValue, l123),
      (l123, -1, l123),
      (l123, 0, l123),
      (l123, 1, List(2,3)),
      (l123, 2, List(3)),
      (l123, 3, Nil),
      (l123, Int.MaxValue, Nil),

      (labc, 1, List("b","c")),
      (labc, 2, List("c")),
      (labc, 3, Nil))
    forAll(tests)(testDrop)
  }

  it should "for all l: List[Int] ==> length(drop(l, n)) == length(l) - n" in {
    forAll("l") { l: List[Int] =>
      val len = length(l)
      forAll (Gen.chooseNum(0, len)) { n: Int =>
        assertResult(len - n)(length(drop(l, n)))
      }
    }
  }

  behavior of "3.5 dropWhile"

  it should "work" in {
    def testDropWhileOdd(l: List[Int], expected: List[Int]) = {
      def odd(n: Int) = n % 2 != 0
      assertResult(expected)(dropWhile(l, odd))
    }

    val tests = Table(
      ("l", "dropWhile(l, odd)"),
      (Nil, Nil),
      (List(1, 3, 1, 2), List(2)),
      (List(2, 4), List(2, 4)),
      (List(2, 1), (List(2, 1))))
    forAll(tests)(testDropWhileOdd)
  }

  behavior of "3.6 init"

  it should "throw a RuntimeException when passed an empty List" in {
    intercept[RuntimeException] {
      init(Nil)
    }
  }

  it should "work" in {
    def testInit[A](l: List[A], expected: List[A]) = {
      assertResult(expected)(init(l))
    }

    val tests = Table(
      ("l", "init(l)"),
      (List(1), Nil),
      (List(1, 2, 3), List(1, 2)),
      (List("a"), Nil),
      (List("a", "b", "c"), List("a", "b")))
    forAll(tests)(testInit)
  }

  behavior of "3.9 length"

  it should "work" in {
    def testLength[A](l: List[A], expected: Int) = {
      assertResult(expected)(length(l))
    }

    val tests = Table(
      ("l", "length(l)"),
      (Nil, 0),
      (List(1), 1),
      (List(1, 2, 3), 3),
      (List("a"), 1),
      (List("a", "b", "c"), 3))
    forAll(tests)(testLength)
  }

  behavior of "3.10 foldLeft"

  it should "work" in {
    def testFoldLeft[A](l: List[A], expected: Int) = {
      assertResult(expected)(foldLeft(l, 0)((acc,_) => acc + 1))
    }

    val tests = Table(
      ("l", "foldLeft(l, ...)"),
      (Nil, 0),
      (List(1), 1),
      (List(1, 2, 3), 3),
      (List("a"), 1),
      (List("a", "b", "c"), 3))
    forAll(tests)(testFoldLeft)
  }

  it should "be equivalent to foldRight" in {
    forAll("l") { l: List[Int] =>
      assertResult(foldRight(l, 0)((_,acc) => acc + 1))(foldLeft(l, 0)((acc,_) => acc + 1))
    }
  }

  behavior of "3.11.1 sumViaFoldLeft"

  it should "be ismorphic to sum" in {
    forAll("l") { l: List[Int] =>
      assertResult(sum(l))(sumViaFoldLeft(l))
    }
  }

  behavior of "3.11.2 productViaFoldLeft"

  it should "work" in {
    def testProductViaFoldLeft(l: List[Double], expected: Double) =
      assertResult(expected)(productViaFoldLeft(l))

    val tests = Table(
      ("l", "productViaFoldLeft(l)"),
      (Nil, 1d),
      (List(1d,0d,2d), 0d),
      (List(1d,2d,3d), 6d))
    forAll(tests)(testProductViaFoldLeft)
  }

  behavior of "3.11.3 lengthViaFoldLeft"

  it should "be equivalent to length" in {
    forAll("l") { l: List[Double] =>
      assertResult(length(l))(lengthViaFoldLeft(l))
    }
  }

  behavior of "3.12 reverse"

  it should "work" in {
    def testReverse[A](l: List[A], expected: List[A]) =
      assertResult(expected)(reverse(l))

    val tests = Table(
      ("l", "reverse(l)"),
      (Nil, Nil),
      (List(1,2,3), List(3,2,1)),
      (List("a", "b", "c"), (List("c", "b", "a"))))
    forAll(tests)(testReverse)
  }

  behavior of "3.14.1 appendViaFoldLeft"

  it should "be ismorphic to append" in {
    forAll("l1", "l2") { (l1: List[Int], l2: List[Int]) =>
      assertResult(append(l1, l2))(appendViaFoldLeft(l1, l2))
    }
  }

  behavior of "3.14.2 appendViaFoldRight"

  it should "be ismorphic to append" in {
    forAll("l1", "l2") { (l1: List[Int], l2: List[Int]) =>
      assertResult(append(l1, l2))(appendViaFoldRight(l1, l2))
    }
  }

  behavior of "3.15 concat"

  it should "work" in {
    def testConcat[A](l: List[List[A]], expected: List[A]) =
      assertResult(expected)(concat(l))

    val tests = Table(
      ("l", "concat(l)"),
      (Nil, Nil),
      (List(Nil), Nil),
      (List(Nil, Nil), Nil),
      (List(List(1,2,3)), List(1,2,3)),
      (List(List(1),List(2),List(3)), List(1,2,3)),
      (List(List("a", "b", "c")), (List("a", "b", "c"))),
      (List(List("a"),List("b"),List("c")), List("a","b","c")))
    forAll(tests)(testConcat)
  }

  behavior of "3.16 add1"

  it should "work" in {
    def testAdd1(l: List[Int], expected: List[Int]) =
      assertResult(expected)(add1(l))

    val tests = Table(
      ("l", "add1(l)"),
      (Nil, Nil),
      (List(1, 2, 3), List(2, 3, 4)),
      (List(-1), List(0)))
    forAll(tests)(testAdd1)
  }

  behavior of "3.17 doubleToString"

  it should "work" in {
    def testDoubleToString(l: List[Double], expected: List[String]) =
      assertResult(expected)(doubleToString(l))

    val tests = Table(
      ("l", "doubleToString(l)"),
      (Nil, Nil),
      (List(1d, -2d, 3d), List("1.0", "-2.0", "3.0")),
      (List(-1.234d), List("-1.234")))
    forAll(tests)(testDoubleToString)
  }

  behavior of "3.18 map"

  it should "work" in {
    def testMap[A](l: List[A], expected: List[A]) =
      assertResult(expected)(map(l)(_.toString + "x"))

    val tests = Table(
      ("l", "map(l)(...)"),
      (Nil, Nil),
      (List(1, 2, 3), List("1x", "2x", "3x")),
      (List("a", "b", "c"), List("ax", "bx", "cx")))
    forAll(tests)(testMap)
  }

  behavior of "3.19 filter"

  it should "work" in {
    def testFilter[A](l: List[A], expected: List[A]) =
      assertResult(expected)(filter(l)(_.toString.toInt % 2 == 0))

    val tests = Table(
      ("l", "filter(l)(...)"),
      (Nil, Nil),
      (List(1, 2, 3, 4), List(2, 4)),
      (List("1", "2", "3"), List("2")))
    forAll(tests)(testFilter)
  }

  behavior of "3.20 flatMap"

  it should "work" in {
    def testFlatMap[A](l: List[A], expected: List[A]) =
      assertResult(expected)(flatMap(l)(x => List(x.toString + "x")))

    val tests = Table(
      ("l", "flatMap(l)(...)"),
      (Nil, Nil),
      (List(1, 2, 3), List("1x", "2x", "3x")),
      (List("a", "b", "c"), List("ax", "bx", "cx")))
    forAll(tests)(testFlatMap)
  }

  it should "be equivalent to Scala List function" in {
    forAll(arbListTuple[Int].arbitrary) { case (l, sl) =>
      assertResult(toList(sl.flatMap(x => SList(x + "x"))))(flatMap(l)(x => List(x + "x")))
    }
  }

  behavior of "3.21 filterViaFlatMap"

  it should "work" in {
    def testFilterViaFlatMap[A](l: List[A], expected: List[A]) =
      assertResult(expected)(filterViaFlatMap(l)(_.toString.toInt % 2 == 0))

    val tests = Table(
      ("l", "filter(l)(...)"),
      (Nil, Nil),
      (List(1, 2, 3, 4), List(2, 4)),
      (List("1", "2", "3"), List("2")))
    forAll(tests)(testFilterViaFlatMap)
  }

  behavior of "3.22 addPairwise"

  it should "work" in {
    def testAddPairwise(l1: List[Int], l2: List[Int], expected: List[Int]) =
      assertResult(expected)(addPairwise(l1, l2))

    val tests = Table(
      ("l1", "l2", "filter(l1,l2)"),
      (Nil, Nil, Nil),
      (List(1, 2, 3), List(4, 5, 6), List(5, 7, 9)),
      (List(1,2,3), List(4,5), List(5,7)))
    forAll(tests)(testAddPairwise)
  }

  behavior of "3.23 zipWith"

  it should "work" in {
    def testZipWith(l1: List[Int], l2: List[String], expected: List[Int]) =
      assertResult(expected)(zipWith(l1, l2){case (a,b) => a + b.toInt})

    val tests = Table(
      ("l1", "l2", "filter(l1,l2)"),
      (Nil, Nil, Nil),
      (List(1, 2, 3), List("4", "5", "6"), List(5, 7, 9)),
      (List(1,2,3), List("4","5"), List(5,7)))
    forAll(tests)(testZipWith)
  }

  behavior of "3.24 hasSubsequence"

  it should "work" in {
    def testHasSubsequence[A](l: List[A], sub: List[A], expected: Boolean) =
      assertResult(expected)(hasSubsequence(l, sub))

    val tests = Table(
      ("l", "sub", "hasSubsequence(l, sub)"),
      (Nil, Nil, false),
      (List(1, 2, 3, 4), Nil, true),
      (List(1, 2, 3, 4), List(1,2), true),
      (List(1, 2, 3, 4), List(2,3), true),
      (List(1, 2, 3, 4), List(4), true),
      (List(1, 2, 3, 4), List(3,2), false))
    forAll(tests)(testHasSubsequence)
  }

}

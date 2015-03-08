package fpinscala.gettingstarted

import scala.Array.canBuildFrom
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Sorting

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

import MyModule.fib
import PolymorphicFunctions.compose
import PolymorphicFunctions.curry
import PolymorphicFunctions.isSorted
import PolymorphicFunctions.partial1
import PolymorphicFunctions.uncurry

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class GettingStartedSpec extends FlatSpec with PropertyChecks {

  behavior of "2.1 fib"

  it should "work" in {
    val tests = Table(
      ("n", "fib(n)"),
      (0, 0), (1, 1), (2, 1), (3, 2), (4, 3), (5, 5), (6, 8), (7, 13))
    forAll(tests) { (x: Int, y: Int) =>
      assertResult(y)(fib(x))
    }
  }

  it should "be the sum of the previous two fibs" in {
    forAll(Gen.chooseNum(2, 100) :| "n") { n: Int =>
      assertResult(fib(n - 1) + fib(n - 2))(fib(n))
    }
  }

  behavior of "2.2 isSorted"

  // make use of Scala's built-in Orderings
  import scala.math.Ordering.Implicits._
  def gt[A: Ordering](x: A, y: A) = x >= y

  it should "work" in {

    def tableTest[A: Ordering](gt: (A, A) => Boolean)(x: Array[A], expected: Boolean): Unit = {
      assertResult(expected)(isSorted(x, gt))
    }

    val testsInt = Table(
      ("as", "expected"),
      (Array[Int](), true),
      (Array(0), true),
      (Array(0, 0), true),
      (Array(0, 1), true),
      (Array(0, 1, 2), true),
      (Array(0, 2, 1), false))
    forAll(testsInt)(tableTest(gt[Int]))

    val testsString = Table(
      ("as", "y"),
      (Array[String](), true),
      (Array("0"), true),
      (Array("0", "0"), true),
      (Array("0", "1"), true),
      (Array("0", "1", "2"), true),
      (Array("0", "2", "1"), false))
    forAll(testsString)(tableTest(gt[String]))
  }

  it should "work for random arrays" in {
    forAll("array") { as: Array[Int] =>
      def toSorted = { val sortedArray = as.clone; Sorting.quickSort(sortedArray); sortedArray }
      val sortedArray = toSorted
      def isAlreadySorted = as.toSeq == sortedArray.toSeq

      assertResult(isAlreadySorted)(isSorted(as, gt[Int]))
      assertResult(true)(isSorted(sortedArray, gt[Int]))
    }
  }

  behavior of "partial1"

  val plus = (_:Int) + (_:Int) //(x: Int, y: Int) => x + y
  val curriedPlus = plus.curried //(x: Int) => (y: Int) => x + y

  val append = (_:String) + (_:String)
  val curriedAppend = append.curried

  def asTuple[A,B] = (_:A, _:B)
  val curriedAsTuple = asTuple.curried

  it should "work" in {
    assertResult(4)(partial1(1, plus)(3))
    assertResult("hello world")(partial1("hello ", append)("world"))
    assertResult((42," is the answer"))(partial1(42, asTuple[Int,String])(" is the answer"))
  }

  it should "work for random Ints" in {
    forAll("x", "y") { (x: Int, y: Int) =>
      assertResult(plus(x,y))(partial1(x, plus)(y))
    }
  }

  it should "work for random Strings" in {
    forAll("x", "y") { (x: String, y: String) =>
      assertResult(append(x,y))(partial1(x, append)(y))
    }
  }

  it should "work for random Ints and Strings" in {
    forAll("x", "y") { (x: Int, y: String) =>
      assertResult(asTuple(x,y))(partial1(x, asTuple[Int,String])(y))
    }
  }

  // curried to assist the compiler - yes, even Batman needs help sometimes!
  def checkForAll[A,B,C](f: (A,B) => C)(toTest: ((A,B) => C,A,B) => C)(implicit a: Arbitrary[A], b: Arbitrary[B]) = {
    forAll("x", "y") { (x: A, y: B) =>
      assertResult(f(x,y))(toTest(f,x,y))
    }
  }

  it should "work for random Strings and Ints" in {
    def toTest[A,B,C](f: (A,B) => C, x: A, y: B): C = partial1(x,f)(y)
    checkForAll(plus)(toTest)
    checkForAll(append)(toTest)
    checkForAll(asTuple[Int,String])(toTest)
  }

  behavior of "2.3 curry"

  it should "add 1 + 3 (1)" in {
    assertResult(4)(curry(plus)(1)(3))
  }

  it should "add two random numbers" in {
    forAll("x", "y") { (x: Int, y: Int) =>
      assertResult(x + y)(curry(plus)(x)(y))
      assertResult(x + y)(curry(plus)(y)(x))
    }
  }

  it should "work for random Strings and Ints" in {
    def toTest[A,B,C](f: (A,B) => C, x: A, y: B): C = curry(f)(x)(y)
    checkForAll(plus)(toTest)
    checkForAll(append)(toTest)
    checkForAll(asTuple[Int,String])(toTest)
  }

  behavior of "2.4 uncurry"

  it should "add 1 + 3 (2)" in {
    assertResult(4)(uncurry(curriedPlus)(1, 3))
  }

  it should "add two random numbers" in {
    forAll("x", "y") { (x: Int, y: Int) =>
      assertResult(x + y)(uncurry(curriedPlus)(x, y))
      assertResult(x + y)(uncurry(curriedPlus)(y, x))
    }
  }

  it should "work for random Strings and Ints" in {
    def toTest[A,B,C](f: (A,B) => C, x: A, y: B): C = uncurry(f.curried)(x,y)
    checkForAll(plus)(toTest)
    checkForAll(append)(toTest)
    checkForAll(asTuple[Int,String])(toTest)
  }

  behavior of "curry-uncurry"

  it should "add 1 + 3" in {
    assertResult(4)(curry(uncurry(curriedPlus))(1)(3))
    assertResult(4)(curry(uncurry(curriedPlus))(3)(1))
  }

  it should "always give the same result" in {
    forAll("x", "y") { (x: Int, y: Int) =>
      assertResult(x + y)(curry(uncurry(curriedPlus))(x)(y))
    }
  }

  it should "work for random Strings and Ints" in {
    def toTest[A,B,C](f: (A,B) => C, x: A, y: B): C = curry(uncurry(f.curried))(x)(y)
    checkForAll(plus)(toTest)
    checkForAll(append)(toTest)
    checkForAll(asTuple[Int,String])(toTest)
  }

  behavior of "uncurry-curry"

  it should "work" in {
    assertResult(4)(uncurry(curry(plus))(1, 3))
  }

  it should "work for random Strings and Ints" in {
    def toTest[A,B,C](f: (A,B) => C, x: A, y: B): C = uncurry(curry(f))(x,y)
    checkForAll(plus)(toTest)
    checkForAll(append)(toTest)
    checkForAll(asTuple[Int,String])(toTest)
  }

  behavior of "2.5 compose"

  def toString[T](t: T) = t.toString
  def neg[T](t: T)(implicit num: Numeric[T]) = num.negate(t)

  it should "work" in {
    assertResult("-42")(compose(toString[Int], neg[Int])(42))
    assertResult("-42.123")(compose(toString[Double], neg[Double])(42.123d))
  }

  it should "work for random Ints" in {
    forAll("x") { (x: Int) =>
      assertResult((-x).toString)(compose(toString[Int], neg[Int])(x))
    }
  }

  it should "work for random Doubles" in {
    forAll("x") { (x: Double) =>
      assertResult((-x).toString)(compose(toString[Double], neg[Double])(x))
    }
  }
}
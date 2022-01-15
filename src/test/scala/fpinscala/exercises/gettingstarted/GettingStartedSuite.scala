package fpinscala.exercises.gettingstarted

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.gettingstarted.MyProgram.fib
import fpinscala.exercises.gettingstarted.PolymorphicFunctions.{compose, curry, isSorted, uncurry}
import fpinscala.exercises.munit.PropSuite

class GettingStartedSuite extends PropSuite:
  private val theFirst21FibonacciNumbers =
    IndexedSeq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765)

  private lazy val mulCurry: Int => Int => Int = curry[Int, Int, Int]((a: Int, b: Int) => a * b)

  private lazy val mulUncurry: (Int, Int) => Int = uncurry[Int, Int, Int]((a: Int) => (b: Int) => a * b)

  private val genShortNumber = Gen.choose(0, 20)

  private val genSortedArray: Gen[Array[Int]] =
    for {
      n <- genShortNumber
      list <- Gen.listOfN(n, genShortNumber)
    } yield list.sorted.toArray

  private val genUnsortedArray: Gen[Array[Int]] =
    for {
      n <- Gen.choose(2, 20)
      list <- Gen.listOfN(n, genShortNumber)
    } yield list.zipWithIndex.map { case (num, i) =>
      if i % 2 == 0 then num + 100
      else num - 100
    }.toArray

  test("MyProgram.fib")(genShortNumber) { i =>
    assertEquals(fib(i), theFirst21FibonacciNumbers(i))
  }

  test("PolymorphicFunctions.isSorted for sorted array case")(genSortedArray) { array =>
    assert(isSorted[Int](array, _ > _))
  }

  test("PolymorphicFunctions.isSorted for unsorted array case")(genUnsortedArray) { array =>
    assert(!isSorted[Int](array, _ > _))
  }

  test("PolymorphicFunctions.curry")(Gen.int ** Gen.int) { case (n, m) =>
    assertEquals(mulCurry(n)(m), n * m)
  }

  test("PolymorphicFunctions.uncurry")(Gen.int ** Gen.int) { case (n, m) =>
    assertEquals(mulUncurry(n, m), n * m)
  }

  test("PolymorphicFunctions.compose")(Gen.int ** Gen.int) { case (n, m) =>
    def aToC = compose[Int, Int, Int]((b: Int) => n * b, (a: Int) => m * a)
    assertEquals(aToC(1), n * m)
  }

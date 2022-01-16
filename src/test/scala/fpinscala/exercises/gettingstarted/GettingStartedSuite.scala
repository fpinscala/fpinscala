package fpinscala.exercises.gettingstarted

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.gettingstarted.MyProgram.fib
import fpinscala.exercises.gettingstarted.PolymorphicFunctions.{compose, curry, isSorted, uncurry}

class GettingStartedSuite extends PropSuite:
  private lazy val mulCurry: Int => Int => Int = curry[Int, Int, Int]((a: Int, b: Int) => a * b)

  private lazy val mulUncurry: (Int, Int) => Int = uncurry[Int, Int, Int]((a: Int) => (b: Int) => a * b)

  private val genSortedArray: Gen[Array[Int]] =
    genList(genShortNumber).map(_.sorted.toArray)

  private val genUnsortedArray: Gen[Array[Int]] =
    for {
      n <- Gen.choose(2, 20)
      list <- Gen.listOfN(n, genShortNumber)
    } yield list.zipWithIndex.map { case (num, i) =>
      if i % 2 == 0 then num + 100
      else num - 100
    }.toArray

  test("MyProgram.fib")(genLengthOfFibonacciSeq) { i =>
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

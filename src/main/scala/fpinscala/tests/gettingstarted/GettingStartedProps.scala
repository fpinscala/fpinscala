package fpinscala.tests.gettingstarted

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.gettingstarted.MyProgram.fib
import fpinscala.exercises.gettingstarted.PolymorphicFunctions.{compose, curry, isSorted, uncurry}

object GettingStartedProps:
  private val theFirst21FibonacciNumbers =
    IndexedSeq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765)
  private val genShortNumber = Gen.choose(0, 20)

  private val fibProp: Prop =
    forAll(genShortNumber)(i => fib(i) == theFirst21FibonacciNumbers(i))
      .tag("MyProgram.fib")

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

  private val isSortedProp: Prop =
    forAll(genSortedArray)(array => isSorted[Int](array, _ > _)).tag("PolymorphicFunctions.isSorted") &&
      forAll(genUnsortedArray)(array => !isSorted[Int](array, _ > _)).tag("not PolymorphicFunctions.isSorted")

  private val curryProp: Prop =
    def mul: Int => Int => Int = curry[Int, Int, Int]((a: Int, b: Int) => a * b)
    forAll(Gen.int ** Gen.int) { case (n, m) => mul(n)(m) == n * m }
      .tag("PolymorphicFunctions.curry")

  private val uncurryProp: Prop =
    def mul: (Int, Int) => Int = uncurry[Int, Int, Int]((a: Int) => (b: Int) => a * b)
    forAll(Gen.int ** Gen.int) { case (n, m) => mul(n, m) == n * m }
      .tag("PolymorphicFunctions.uncurry")

  private val composeProp: Prop =
    forAll(Gen.int ** Gen.int) { case (n, m) =>
      def aToC = compose[Int, Int, Int]((b: Int) => n * b, (a: Int) => m * a)
      aToC(1) == n * m
    }.tag("PolymorphicFunctions.compose")

  @main def checkGettingStarted(): Unit =
    fibProp.run()
    isSortedProp.run()
    curryProp.run()
    uncurryProp.run()
    composeProp.run()

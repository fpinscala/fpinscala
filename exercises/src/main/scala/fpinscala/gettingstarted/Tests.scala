package fpinscala.gettingstarted

import fpinscala.gettingstarted.MyProgram.*
import fpinscala.gettingstarted.PolymorphicFunctions.*
import fpinscala.gettingstarted.Tests.*
import org.scalacheck.*

@main def checkAll(): Unit = Tests.check()

private object Tests extends Properties("fpinscala.gettingstarted"):
  import Gen.*
  import Prop.forAll

  private val theFirst21FibonacciNumbers =
    IndexedSeq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765)
  private val numberLessThan21 = Gen.choose(0, 20)

  property("MyProgram.fib") = forAll(numberLessThan21)(i => fib(i) == theFirst21FibonacciNumbers(i))

  private val genSortedArray =
    Gen.containerOf[Array, Int](numberLessThan21).map(_.sorted)
  private val genUnsortedArray =
    Gen.containerOf[Array, Int](numberLessThan21).suchThat(array => !(array sameElements array.sorted))

  property("PolymorphicFunctions.isSorted") =
    forAll(genSortedArray)(array => isSorted[Int](array, _ > _)) ++
    forAll(genUnsortedArray)(array => !isSorted[Int](array, _ > _))

  private val genSmallNumbers = for {
    n <- Gen.choose(0, 1000)
    m <- Gen.choose(0, 1000)
  } yield (n, m)

  property("PolymorphicFunctions.curry") =
    def mul: Int => Int => Int = curry[Int, Int, Int]((a: Int, b: Int) => a * b)
    forAll(genSmallNumbers) { case (n, m) => mul(n)(m) == n * m }

  property("PolymorphicFunctions.uncurry") =
    def mul: (Int, Int) => Int = uncurry[Int, Int, Int]((a: Int) => (b: Int) => a * b)
    forAll(genSmallNumbers) { case (n, m) => mul(n, m) == n * m }

  property("PolymorphicFunctions.compose") =
    forAll(genSmallNumbers) { case (n, m) =>
      def aToC = compose[Int, Int, Int]((b: Int) => n * b, (a: Int) => m * a)
      aToC(1) == n * m
    }

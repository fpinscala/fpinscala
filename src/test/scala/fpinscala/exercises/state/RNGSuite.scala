package fpinscala.exercises.state

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.state.RNG
import fpinscala.exercises.state.RNG.*

import scala.annotation.tailrec

class RNGSuite extends PropSuite:
  private val genRNG = Gen.int.map(Simple(_))
  private val genCounter = Gen.choose(10, 100)
  private val genLengthOfList = Gen.choose(-5, 20)
  private val genSmallPosNum = Gen.choose(1, 1000)
  private val isInInterval: Double => Boolean = d => 0 <= d && d < 1

  test("RNG.nextInt")(genRNG) { rng =>
    val (n1, rng2) = rng.nextInt
    val (n2, rng3) = rng2.nextInt
    val (n3, _) = rng3.nextInt
    val (n4, _) = rng.nextInt
    val (n5, _) = rng2.nextInt
    assertNotEquals(n1, n2)
    assertNotEquals(n1, n3)
    assertEquals(n1, n4)
    assertNotEquals(n2, n3)
    assertEquals(n2, n5)
  }

  // Check that the first n-th numbers of the rng aren't a negative number?
  test("RNG.nonNegativeInt")(genRNG ** genCounter) { case (rng, counter) =>
    assert(checkRND(rng, counter, nonNegativeInt, _ >= 0))
  }

  // Check that the first n-th numbers of the rng are in the interval [0, 1)
  test("RNG.double")(genRNG ** genCounter) { case (rng, counter) =>
    assert(checkRND(rng, counter, double, isInInterval))
  }

  test("RNG.intDouble")(genRNG ** genCounter) { case (rng, counter) =>
    assert(checkRND(rng, counter, intDouble, (_, d) => isInInterval(d)))
  }

  test("RNG.doubleInt")(genRNG ** genCounter) { case (rng, counter) =>
    assert(checkRND(rng, counter, doubleInt, (d, _) => isInInterval(d)))
  }

  test("RNG.double3")(genRNG ** genCounter) { case (rng, counter) =>
    val isCorrect: ((Double, Double, Double)) => Boolean =
      (d1, d2, d3) => isInInterval(d1) && isInInterval(d2) && isInInterval(d3) && d1 != d2 && d1 != d3 && d2 != d3
    assert(checkRND(rng, counter, double3, isCorrect))
  }

  test("RNG.ints")(genRNG ** genCounter ** genLengthOfList) { case ((rng, counter), lengthOfList) =>
    if lengthOfList <= 0 then assert(ints(lengthOfList)(rng)._1.isEmpty)
    else assert(checkRND(rng, counter, ints(lengthOfList), list => list == list.distinct))
  }

  test("RNG.int")(genRNG ** genCounter) { case (rng, counter) =>
    assert(checkRND(rng, counter, int, _ => true))
  }

  test("RNG.unit")(genRNG ** genCounter) { case (rng, counter) =>
    assert(checkRNGUnit(rng, counter, "unit"))
    assert(checkRNGUnit(rng, counter, 0))
    assert(checkRNGUnit(rng, counter, 0.0))
  }

  test("RNG.map")(genRNG ** genCounter) { case (rng, counter) =>
    assert(checkRND(rng, counter, map(int)(_.toString), _.toIntOption.isDefined))
  }

  /*
  test("RNG._double")(genRNG ** genCounter) { case (rng, counter) =>
    assert(checkRND(rng, counter, _double, isInInterval))
  }
   */

  test("RNG.map2")(genRNG ** genCounter) { case (rng, counter) =>
    val randC = map2(double, double)((d1, d2) => (d1, d2))
    assert(checkRND(rng, counter, randC, (d1, d2) => isInInterval(d1) && isInInterval(d2) && d1 != d2))
  }

  test("RNG.sequence")(genRNG ** genCounter ** genLengthOfList) { case ((rng, counter), lengthOfList) =>
    val ints: Rand[List[Int]] = sequence(List.fill(lengthOfList)(int))
    if lengthOfList <= 0 then assert(ints(rng)._1.isEmpty)
    else assert(checkRND(rng, counter, ints, list => list == list.distinct))
  }

  test("RNG.flatMap")(genRNG ** genCounter ** genSmallPosNum) { case ((rng, counter), limit) =>
    def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    assert(checkRNGNonNegativeLessThan(rng, counter, nonNegativeLessThan(limit), limit))
  }

  test("RNG.mapViaFlatMap")(genRNG ** genCounter) { case (rng, counter) =>
    assert(checkRND(rng, counter, mapViaFlatMap(int)(_.toString), _.toIntOption.isDefined))
  }

  test("RNG.map2ViaFlatMap")(genRNG ** genCounter) { case (rng, counter) =>
    val randC = map2ViaFlatMap(double, double)((d1, d2) => (d1, d2))
    assert(checkRND(rng, counter, randC, (d1, d2) => isInInterval(d1) && isInInterval(d2) && d1 != d2))
  }

  private def checkRND[A](rng: RNG, counter: Int, generate: Rand[A], isCorrect: A => Boolean): Boolean =
    @tailrec
    def loop(rng: RNG, counter: Int, previousValue: Option[A] = None): Boolean =
      counter < 0 || {
        val (value, rng2) = generate(rng)
        if !isCorrect(value) then fail(s"The generated value '$value' is invalid.")
        else if previousValue.contains(value) then fail(s"The RNG generated two identical values in a row ('$value').")
        else loop(rng2, counter - 1, Some(value))
      }
    loop(rng, counter)

  private def checkRNGUnit[A](rng: RNG, counter: Int, constantValue: A): Boolean =
    @tailrec
    def loop(rng: RNG, counter: Int): Boolean =
      counter < 0 || {
        val (value, rng2) = RNG.unit(constantValue)(rng)
        if constantValue == value then loop(rng2, counter - 1)
        else fail(s"RNG.unit must always return a constant value ('$constantValue').")
      }
    loop(rng, counter)

  private def checkRNGNonNegativeLessThan(rng: RNG, counter: Int, generate: Rand[Int], limit: Int): Boolean =
    @tailrec
    def loop(rng: RNG, counter: Int): Boolean =
      counter < 0 || {
        val (i, rng2) = generate(rng)
        if 0 <= i && i < limit then loop(rng2, counter - 1)
        else fail(s"The generated value '$i' isn't in the interval [0, $limit).")
      }
    loop(rng, counter)

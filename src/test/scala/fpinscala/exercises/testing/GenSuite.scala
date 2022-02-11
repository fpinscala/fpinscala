package fpinscala.exercises.testing

import fpinscala.answers.testing.exhaustive.Gen as ExhGen
import fpinscala.answers.testing.exhaustive.Gen.`**`
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.state.RNG
import fpinscala.exercises.state.State.*
import fpinscala.exercises.testing.Gen
import fpinscala.exercises.testing.Gen.*
import fpinscala.exercises.testing.Prop.*

class GenSuite extends PropSuite:
  private val shortSample = 1000
  private val genRNG: ExhGen[RNG] = ExhGen.int.map(i => RNG.Simple(i.toLong))

// Gen tests: Before using these tests (starting from Exercise 8.4),
// add the next block to fpinscala.exercises.testing.Gen.scala file
/* ToDo: fpinscala.exercises.testing.Gen.scala file's block
opaque type Gen[+A] = State[RNG, A]

object Gen:
  extension [A](self: Gen[A])
    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = self.run(rng)
*/

// Gen tests:
/*
  test("Exercise 8.4")(ExhGen.int ** ExhGen.int ** genRNG) { case n ** m ** rng =>
    val (start, stopExclusive) = if n < m then (n, m) else (m, n)
    val (k, _) = Gen.choose(start, stopExclusive).next(rng)
    assert(start <= k && k <= stopExclusive)
  }

  test("Exercise 8.5, unit")(ExhGen.int ** genRNG) { case n ** rng0 =>
    val genUnit = Gen.unit(n)
    val (n1, rng1) = genUnit.next(rng0)
    assertEquals(n1, n)
    val (n2, _) = genUnit.next(rng1)
    assertEquals(n2, n)
  }

  test("Exercise 8.5, boolean + listOfN")(genShortNumber ** genRNG) { case n ** rng0 =>
    val (randomBooleanList, rng1) = Gen.boolean.listOfN(shortSample).next(rng0)
    assert(randomBooleanList.contains(true), "'Gen.boolean' should not generate only 'false' values")
    assert(randomBooleanList.contains(false), "'Gen.boolean' should not generate only 'true' values")

    val (randomBooleanList1, _) = Gen.boolean.listOfN(n).next(rng1)
    assertEquals(randomBooleanList1.length, n)
  }

  test("Exercise 8.6, flatMap")(ExhGen.int ** genRNG) { case n ** rng =>
    val genA = Gen.unit(n)
    def aToGenB(a: Int) = Gen.unit(a % 2 == 0)
    val (isEven, _) = genA.flatMap(aToGenB).next(rng)
    assertEquals(n % 2 == 0, isEven)
  }

  test("Exercise 8.6, listOfN")(genShortNumber ** genRNG) { case n ** rng =>
    val (randomBooleanList, _) = Gen.boolean.listOfN(Gen.unit(n)).next(rng)
    assertEquals(randomBooleanList.length, n)
  }

  test("Exercise 8.7")(ExhGen.int ** ExhGen.int ** genRNG) { case n ** m ** rng =>
    val genUnion = Gen.union(Gen.unit(n), Gen.unit(m))
    val genUnionList = genUnion.listOfN(shortSample)
    val (unionList, _) = genUnionList.next(rng)
    assert(unionList.count(_ == n) >= shortSample / 3, "Values should be extracted with approximately equal likelihood")
    assert(unionList.count(_ == m) >= shortSample / 3, "Values should be extracted with approximately equal likelihood")
  }

  test("Exercise 8.8")(ExhGen.int ** ExhGen.int ** genRNG) { case n ** m ** rng =>
    val genUnion0 = Gen.weighted((Gen.unit(n), 0.0), (Gen.unit(m), 1.0))
    val (unionList0, _) = genUnion0.listOfN(shortSample).next(rng)
    assertEquals(unionList0.count(_ == n), 0, "g1 weights 0")
    assertEquals(unionList0.count(_ == m), shortSample, "g1 weights 0")

    val genUnion1 = Gen.weighted((Gen.unit(n), 1.0), (Gen.unit(m), 0.0))
    val (unionList1, _) = genUnion1.listOfN(shortSample).next(rng)
    assertEquals(unionList1.count(_ == n), shortSample, "g2 weights 0")
    assertEquals(unionList1.count(_ == m), 0, "g2 weights 0")

    val genUnion2 = Gen.weighted((Gen.unit(n), 0.5), (Gen.unit(m), 0.5))
    val (unionList2, _) = genUnion2.listOfN(shortSample).next(rng)
    assert(unionList2.count(_ == n) >= shortSample / 3, "g1 and g2 have the same weight")
    assert(unionList2.count(_ == m) >= shortSample / 3, "g1 and g2 have the same weight")

    val genUnion3 = Gen.weighted((Gen.unit(n), 0.33), (Gen.unit(m), 0.67))
    val (unionList3, _) = genUnion3.listOfN(shortSample).next(rng)
    assert(unionList3.count(_ == n) >= shortSample / 5, "g2 is twice as common as g1")
    assert(unionList3.count(_ == m) >= shortSample / 2, "g2 is twice as common as g1")
  }
*/


// Prop tests: Before using these tests (starting from Exercise 8.9),
// add the next block to fpinscala.exercises.testing.Gen.scala file
/* ToDo: fpinscala.exercises.testing.Gen.scala file's block
object Prop:
  extension (self: Prop)
    def check(
               maxSize: MaxSize = 100,
               testCases: TestCases = 100,
               rng: RNG = RNG.Simple(System.currentTimeMillis)
             ): Result =
      self(maxSize, testCases, rng)
*/

// Prop tests
/*
  import fpinscala.exercises.testing.Prop.Result.*

  private val propPassed = Prop((n, rng) => Passed)
  private val falsified = Falsified(FailedCase.fromString("Failed case"), SuccessCount.fromInt(0))
  private val propFalsified = Prop((n, rng) => falsified)

  test("Exercise 8.9")(ExhGen.unit(())) { _ =>
    assertEquals((propPassed && propPassed).check(), Passed)
    assert((propPassed && propFalsified).check().isFalsified)
    assert((propFalsified && propPassed).check().isFalsified)
    assert((propFalsified && propFalsified).check().isFalsified)

    assertEquals((propPassed || propPassed).check(), Passed)
    assertEquals((propPassed || propFalsified).check(), Passed)
    assertEquals((propFalsified || propPassed).check(), Passed)
    assert((propFalsified || propFalsified).check().isFalsified)
  }
*/


// SGen tests: Before using these tests (starting from Exercise 8.10),
// add the next block to fpinscala.exercises.testing.Gen.scala file
/* ToDo: fpinscala.exercises.testing.Gen.scala file's block
object SGen:
  def apply[A](f: Int => Gen[A]): SGen[A] = f

  extension [A](self: SGen[A])
    def apply(n: Int): Gen[A] = self(n)
*/

// SGen tests
/*
  test("Exercises 8.10 + 8.11")(ExhGen.int ** genRNG) { case n ** rng0 =>
    val sGenA = SGen(Gen.unit(_))
    def aToB(a: Int) = a % 2 == 0
    val (isEven0, rng1) = sGenA.map(aToB).apply(n).next(rng0)
    assertEquals(n % 2 == 0, isEven0)

    def aToGenB(a: Int) = Gen.unit(aToB(a)).unsized
    val (isEven1, _) = sGenA.flatMap(aToGenB).apply(n).next(rng1)
    assertEquals(n % 2 == 0, isEven1)
  }

  test("Exercise 8.12")(genShortNumber ** genRNG) { case n ** rng =>
    val (randomBooleanList, _) = Gen.boolean.list(n).next(rng)
    assertEquals(randomBooleanList.length, n)
  }

  test("Exercise 8.13")(genShortNumber ** genRNG) { case n ** rng =>
    val (randomNonEmptyList, _) = Gen.boolean.nonEmptyList(n).next(rng)
    assert(randomNonEmptyList.nonEmpty)
  }
*/

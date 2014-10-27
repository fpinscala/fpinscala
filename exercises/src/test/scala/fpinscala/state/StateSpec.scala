package fpinscala.state

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalacheck.Gen
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import RNG._
import State.simulateMachine
import org.scalacheck.Commands
import org.scalatest.prop.Checkers

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class StateSpec extends FlatSpec with PropertyChecks with Matchers {

  private case class TestRNG(value: Int) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newValue = if (value == Int.MaxValue) Int.MinValue else value + 1
      (value, TestRNG(newValue))
    }
  }

  private def testCornerCases(test: Int => Unit) = {
    val tests = Table(
      ("n: Int"),
      (0), (1), (-1), (Int.MaxValue), (Int.MinValue))
    forAll(tests)(test)
  }

  private def testProperty(test: Int => Unit) =
    forAll("n: Int") { n: Int => test(n) }

  private def testMean[A: Numeric](rand: Rand[A], meanCheck: Double => Unit, numIterations: Int = 100000) = {
    val ev = implicitly[Numeric[A]]
    val rng: RNG = Simple(42)
    val (sumRNs, _) = (1 to numIterations).foldLeft((0d, rng)) {
      case ((sum1, rng1), _) => map(rand)(sum1 + ev.toDouble(_))(rng1)
    }
    val meanRNs = sumRNs / numIterations
    meanCheck(meanRNs)
  }

  private def testUniformDistribution[A: Numeric](rand: Rand[A],
      rng: RNG = Simple(42), numIterations: Int = 100000) =
  {
    import scala.collection.mutable
    // as we currently only use distribution.size a mutable.Set would do, too
    val distribution = mutable.Map[A,Int]()
    def put(a: A) = {
      val oldValue = distribution.getOrElse(a, 0)
      distribution.put(a, oldValue + 1)
    }
    def setDistribution = {
      (1 to numIterations).foldLeft(rng) {
        case (rng1, _) =>
          val (a, rng2) = rand(rng1)
          put(a)
          rng2
      }
    }
    setDistribution
    assert(numIterations - distribution.size <= 2)
  }

  private def rangeCheck(mean: Double, delta: Double)(d: Double) =
    d should (be >= mean - delta and be <= mean + delta)

  behavior of "6.0 Simple.nextInt"
  private def nextInt(rng: RNG): (Int, RNG) = rng.nextInt
  // Simple seems to be heavily biased towards positive Ints?!
  ignore should "have a mean of 0" in testMean(nextInt, rangeCheck(0, 100))
  it should "have a uniform distribution" in testUniformDistribution(nextInt)

  behavior of "6.1 nonNegativeInt"

  private def testNonNegativeInt(n: Int) = nonNegativeInt(TestRNG(n))._1 should be >= 0
  private val NarrowingFactor: Double = 10000d
  private def nonNegativeIntNarrowed(rng: RNG): (Double, RNG) = {
    // narrow the range [0, Int.MaxValue] for less variance of mean value
//      map(nonNegativeInt)(_ / NarrowingFactor)(rng)
      val (i, rng1) = nonNegativeInt(rng)
      (i / NarrowingFactor, rng1)
    }

  it should "work for corner cases" in testCornerCases(testNonNegativeInt)
  it should "return a value >= 0 for all nextInt values" in testProperty(testNonNegativeInt)
  it should "have a mean of Int.MaxValue / 2" in
    testMean(nonNegativeIntNarrowed, rangeCheck(Int.MaxValue / (2 * NarrowingFactor), 100))
  it should "have a uniform distribution" in testUniformDistribution(nonNegativeIntNarrowed)

  behavior of "6.2 double"

  def testDoubleRange(d: Double, offset: Int = 0) = d should (be >= 0d + offset and be < 1d + offset)

  def testDouble(n: Int) = testDoubleRange(double(TestRNG(n))._1)

  it should "work for corner cases" in testCornerCases(testDouble)
  it should "return a value >= 0 and < 1 for all nextInt values" in testProperty(testDouble)
  it should "have a mean of 0.5" in testMean(double, rangeCheck(0.5, 0.01))
  it should "have a uniform distribution" in testUniformDistribution(double)

  behavior of "6.3.1 intDouble"

  def testIntDouble(n: Int) = {
    val ((i, d), _) = intDouble(TestRNG(n))
    testDoubleRange(d)
  }

  it should "work for corner cases" in testCornerCases(testIntDouble)
  it should "return a double value >= 0 and < 1 for all nextInt values" in testProperty(testIntDouble)

  behavior of "6.3.2 doubleInt"

  def testDoubleInt(n: Int) = {
    val ((d, i), _) = doubleInt(TestRNG(n))
    testDoubleRange(d)
  }

  it should "work for corner cases" in testCornerCases(testDoubleInt)
  it should "return a double value >= 0 and < 1 for all nextInt values" in testProperty(testDoubleInt)

  behavior of "6.3.3 double3"

  def testDouble3(n: Int) = {
    val ((d1, d2, d3), _) = double3(TestRNG(n))
    testDoubleRange(d1); testDoubleRange(d2); testDoubleRange(d3)
  }

  it should "work for corner cases" in {
    testCornerCases(testDouble3)
  }

  it should "return a double value >= 0 and < 1 for all nextInt values" in {
    testProperty(testDouble3)
  }

  behavior of "6.4 ints"

  def testInts(count: Int)(n: Int) = {
    val (is, _) = ints(count)(TestRNG(n))
    is should have length count
  }

  it should "work for corner cases" in {
    testCornerCases(testInts(0))
    testCornerCases(testInts(1))
    testCornerCases(testInts(10))
  }

  it should "return a double value >= 0 and < 1 for all nextInt values" in {
    forAll(Gen.chooseNum(0, 1) :| "count: Int") { count: Int =>
      testProperty(testInts(count))
    }
  }

  behavior of "6.5 doubleViaMap"

  def testDoubleViaMap(n: Int) = testDoubleRange(doubleViaMap(TestRNG(n))._1)

  it should "work for corner cases" in testCornerCases(testDoubleViaMap)
  it should "return a value >= 0 and < 1 for all nextInt values" in testProperty(testDoubleViaMap)

  behavior of "6.6 map2"

  def testMap2(n: Int) = {
    val (d, _) = map2(double, double)(_ * _)(TestRNG(n))
    testDoubleRange(d)
  }

  it should "work for corner cases" in testCornerCases(testMap2)
  it should "return a value >= 0 and < 1 for all nextInt values" in testProperty(testMap2)

  behavior of "6.7 RNG.sequence"

  def testSequence(count: Int)(n: Int) = {
    val seq = List.fill(count)(int)
    val (is, _) = RNG.sequence(seq)(TestRNG(n))
    is should have length count
  }

  it should "work for corner cases" in {
    testCornerCases(testSequence(0))
    testCornerCases(testSequence(1))
    testCornerCases(testSequence(10))
  }

  it should "return a double value >= 0 and < 1 for all nextInt values" in {
    forAll(Gen.chooseNum(0, 1) :| "count: Int") { count: Int =>
      testProperty(testSequence(count))
    }
  }

  behavior of "6.8.1 flatMap"

  def testFlatMap(n: Int) = {
    val (d,_) = flatMap(double)(rd => rng => (42 + rd, rng))(TestRNG(n))
    testDoubleRange(d, 42)
  }

  it should "work for corner cases" in testCornerCases(testFlatMap)
  it should "return a value >= 42.0 and < 43.0 for all nextInt values" in testProperty(testFlatMap)

  behavior of "6.8.2 nonNegativeLessThan"

  def testNonNegativeLessThan(n: Int) = {
    val (i,_) = nonNegativeLessThan(42)(TestRNG(n))
    i should (be >= 0 and be < 42)
  }

  it should "work for corner cases" in testCornerCases(testNonNegativeLessThan)
  it should "return a value >= 0 and < 42 for all nextInt values" in
    testProperty(testNonNegativeLessThan)

  behavior of "6.9.1 mapViaFlatMap"

  def testMapViaFlatMap(n: Int) = {
    val (d, _) = mapViaFlatMap(double)(_ + 42)(TestRNG(n))
    testDoubleRange(d, 42)
  }

  it should "work for corner cases" in testCornerCases(testMapViaFlatMap)
  it should "return a value >= 0 and < 1 for all nextInt values" in testProperty(testMapViaFlatMap)

  behavior of "6.9.2 RNG.map2ViaFlatMap"

  def testMap2ViaFlatMap(n: Int) = {
    val (d, _) = map2ViaFlatMap(double, double)(_ * _)(TestRNG(n))
    testDoubleRange(d)
  }

  it should "work for corner cases" in testCornerCases(testMap2ViaFlatMap)
  it should "return a value >= 0 and < 1 for all nextInt values" in testProperty(testMap2ViaFlatMap)

  behavior of "6.10.1 State.unit"

  def testStateUnit(n: Int) = {
    assertResult((n, 42))(State.unit(n).run(42))
  }

  it should "always result in the value that was passed in" in testProperty(testStateUnit)

  behavior of "6.10.2 State.map"

  def createState(n: Int) = State[Int,Int](s => (n, s + 1))

  def testStateMap(n: Int) = {
    val (i, s) = createState(n).map(_.toString).run(n)
    assertResult(n.toString)(i)
    assertResult(n + 1)(s)
  }

  it should "work" in assertResult((3, 42))(State.unit(1).map(_ + 2).run(42))
  it should "always result in (n.toString,n+1)" in testProperty(testStateMap)

  behavior of "6.10.2 State.map2"

  def testStateMap2(n: Int) = {
    val (i, s) = createState(n).map2(createState(n))(_ + _).run(n)
    assertResult(n * 2)(i)
    assertResult(n + 2)(s)
  }

  it should "always result in (n*2,n+2)" in testProperty(testStateMap2)

  behavior of "6.10.4 State.flatMap"

  def testStateFlatMap(n: Int) = {
    val (i, s) = createState(n).flatMap(createState).run(n)
    assertResult(n)(i)
    assertResult(n + 2)(s)
  }

  it should "always result in (n,n+2)" in testProperty(testStateFlatMap)
  it should "make for-comprehension work" in {
    def testForComprehension(n: Int) = {
      val s1 = createState(n)
      val s2 = createState(n)
      val s3 = for {
        n1 <- s1
        n2 <- s2
      } yield(n1 + n2)
      val (n4, s4) = s3.run(n)
    assertResult(n)(n4)
    assertResult(n + 2)(s4)
    }
  }

  behavior of "6.10.5 State.sequence"

  def testStateSequence(n: Int) = {
    val (i, s) = State.sequence(List(createState(n), createState(n))).run(n)
    assertResult(List(n,n))(i)
    assertResult(n + 2)(s)
  }

  it should "always result in (List(n,n),n+2)" in testProperty(testStateSequence)

  behavior of "6.11 simulateMachine"

  it should "follow the rules" in {
    val ruleTests = Table(
      ("rule",
         "inputs",          "Machine before",    "Machine after",       "expected result"),
      ("rule 1: insert coin into locked machine w/ candy -> unlock",
          List[Input](Coin), Machine(true, 1, 0), Machine(false, 1, 1), (1,1)),
      ("rule 2: turn knob on unlocked machine -> dispense candy and lock",
          List[Input](Turn), Machine(false, 1, 0), Machine(true, 0, 0), (0,0)),
      ("rule 3a: turn knob on locked machine -> no effect",
          List[Input](Turn), Machine(true, 1, 0), Machine(true, 1, 0), (0,1)),
      ("rule 3b: insert coin into unlocked machine -> no effect",
          List[Input](Coin), Machine(false, 1, 0), Machine(false, 1, 0), (0,1)),
      ("rule 4a: turn knob on machine w/o candy -> no effect",
          List[Input](Turn), Machine(false, 0, 0), Machine(false, 0, 0), (0,0)),
      ("rule 4b: insert coin into machine w/o candy -> no effect",
          List[Input](Coin), Machine(true, 0, 0), Machine(true, 0, 0), (0,0))
    )
    forAll(ruleTests) {
      (rule: String, inputs: List[Input], machineBefore: Machine, machineAfter: Machine, expected: (Int,Int)) =>
        assertResult((expected, machineAfter), rule) {
          simulateMachine(inputs).run(machineBefore)
        }
    }
  }

  it should "behave as the example in the book describes" in {
    val buyOneCandy = List[Input](Coin, Turn)
    def buyCandies(times: Int): List[Input] = (List.fill(times)(buyOneCandy)).flatten

    assertResult(((14,1), Machine(true, 1, 14)), "see example in the book") {
        simulateMachine(buyCandies(4)).run(Machine(true, 5, 10))
    }
  }

  it should "have the right state transitions" in {
    object CandyMachineSpecification extends Commands {
      var currentMachine: Machine = _

      case class State(locked: Boolean, candies: Int, coins: Int)

      private def asState(machine: Machine) = State(machine.locked, machine.candies, machine.coins)

      override def initialState = {
        currentMachine = Machine(true, 10, 0)
        asState(currentMachine)
      }

      case object InsertCoin extends Command {
        postConditions += {
          case (s0, s1, r:(Int,Int)) =>
            r._1 == s1.coins && r._2 == s1.candies &&
            s0.candies >= 0 && s0.candies == s1.candies && s0.coins <= s1.coins
          case _ => false
        }
        override def run(s: State) = {
          val (ret, nextMachine) = simulateMachine(List[Input](Coin)).run(currentMachine)
          currentMachine = nextMachine
          ret
        }
        override def nextState(s: State) = {
          if (s.candies < 1 || !s.locked) s else State(false, s.candies, s.coins + 1)
        }
      }

      case object TurnKnob extends Command {
        postConditions += {
          case (s0, s1, r:(Int,Int)) =>
            r._1 == s1.coins && r._2 == s1.candies &&
            s0.candies >= 0 && s0.candies >= s1.candies && s1.candies >= 0 && s0.coins == s1.coins
          case _ => false
        }
        override def run(s: State) = {
          val (ret, nextMachine) = simulateMachine(List[Input](Turn)).run(currentMachine)
          currentMachine = nextMachine
          ret
        }
        override def nextState(s: State) = {
          if (s.candies < 1 || s.locked) s else State(true, s.candies - 1, s.coins)
        }
      }

      override def genCommand(s: State): Gen[Command] = Gen.oneOf(InsertCoin, TurnKnob)
    }

    Checkers.check(CandyMachineSpecification)
  }
}

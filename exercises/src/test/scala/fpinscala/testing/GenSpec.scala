package fpinscala.testing

import org.junit.runner.RunWith
import org.scalacheck.{Gen => SCGen}
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import fpinscala.state.RNG
import org.scalatest.BeforeAndAfterEach
import Prop._
import fpinscala.parallelism.Par

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class GenSpec extends FlatSpec with PropertyChecks with BeforeAndAfterEach {

  var rng: RNG = _

  implicit class TestGenOps[A](gen: Gen[A]) {
    def get: A = {
      val (a, nextRng) = gen.sample.run(rng)
      rng = nextRng
      a
    }
  }

  implicit class TestPropOps(prop: Prop) {
    def get: Result = prop.run(1, rng)
  }

  override def beforeEach =
    rng = RNG.Simple(0)

  private val between0And100 = SCGen.chooseNum(0, 100) label "n"
  private val intListGen = SCGen.listOf(between0And100) label "ints"

  behavior of "8.1 List.sum"
  it should "obey some laws" in {
      forAll(intListGen) { ints =>
        ???
      }
  }

  behavior of "8.2 List.max"
  it should "obey some laws" in {
      forAll(intListGen) { ints =>
        ???
      }
  }

  behavior of "8.3 Prop0.&&"
  it should "work" in {
    def asProp0(b: Boolean): Prop0 = new Prop0 {override def check = b}
    def testAnd(check1: Boolean, check2: Boolean, expected: Boolean) =
      assertResult(expected)((asProp0(check1) && asProp0(check2)).check)

    val tests = Table(
      ("prop1.check", "prop2.check", "(prop1 && prop2).check"),
      (true, true, true),
      (true, false, false),
      (false, true, false),
      (false, false, false)
    )
    forAll(tests)(testAnd)
  }

  behavior of "8.4 Gen.choose"
  it should "should stay within specified range" in {
    val startInts = SCGen.choose(-100, 100)
    forAll(startInts label "start") { start =>
      forAll(SCGen.choose(start + 1, start + 101) label "stopExclusive") { stopExclusive =>
        val i = Gen.choose(start, stopExclusive).get
//        println(s"start=$start,stopExclusive=$stopExclusive,i=$i")
        assert(i >= start && i < stopExclusive)//, s"$i >= $start && $i < $stopExclusive")
      }
    }
  }

  behavior of "8.5.1 Gen.unit"
  it should "always return the same object" in {
    forAll("a") { a: Int =>
      assert(a == Gen.unit(a).get)
    }
  }

  behavior of "8.5.2 Gen.boolean"
  it should "have an equal distribution" in {
    val trues = Seq.fill(100)(Gen.boolean.get).filter(x => x)
    assert((trues.size - 50).abs <= 10)
  }

  behavior of "8.5.3 Gen.listOfN"
  it should "generate a list of n elements" in {
    forAll(between0And100) { n =>
      assert(Gen.listOfN(n, Gen.unit(0)).get.size == n)
    }
  }

  behavior of "8.6.1 Gen.map"
  it should "work" in {
    forAll("a") { a: Int =>
      assert(a.toString == Gen.unit(a).map(_.toString).get)
    }
  }

  behavior of "8.6.1 Gen.flatMap"
  it should "work" in {
    forAll("a") { a: Int =>
      assert(a == Gen.unit(a).flatMap(Gen.unit(_)).get)
    }
  }

  behavior of "8.6.2 Gen.listOfN"
  it should "work" in {
    forAll(between0And100) { n =>
      assert(List.fill(n)("X") == Gen.unit("X").listOfN(Gen.unit(n)).get)
    }
  }

  behavior of "8.7 Gen.union"
  it should "have an equal distribution" in {
    val booleans = Gen.union(Gen.unit(true), Gen.unit(false))
    val trues = Seq.fill(100)(booleans.get).filter(x => x)
    assert((trues.size - 50).abs <= 10)
  }

  behavior of "8.8 Gen.weighted"
  it should "have the required distribution" in {
    forAll(between0And100) { n =>
      val booleans = Gen.weighted((Gen.unit(true), n), (Gen.unit(false), 100 - n))
      val trues = Seq.fill(100)(booleans.get).filter(x => x)
      assert((trues.size - n).abs <= 15)
    }
  }

  behavior of "8.9.1 Prop.&&"
  it should "work" in {
    def asProp(b: Boolean, msg: String = "bollocks"): Prop =
      Prop((_,_) => if (b) Passed else Falsified(msg, 1))
    def testAnd(check1: Boolean, check2: Boolean, expected: Boolean, msg: String) =
      assertResult(asProp(expected, msg).get)((asProp(check1) && asProp(check2)).get)

    val tests = Table(
      ("prop1.result", "prop2.result", "(prop1 && prop2).result", "msg"),
      (true, true, true, ""),
      (true, false, false, "bollocks"),
      (false, true, false, "bollocks"),
      (false, false, false, "bollocks")
    )
    forAll(tests)(testAnd)
  }

  behavior of "8.9.2 Prop.||"
  it should "work" in {
    def asProp(b: Boolean, msg: String = "bollocks"): Prop =
      Prop((_,_) => if (b) Passed else Falsified(msg, 1))
    def testOr(check1: Boolean, check2: Boolean, expected: Boolean, msg: String) =
      assertResult(asProp(expected, msg).get)((asProp(check1) || asProp(check2)).get)

    val tests = Table(
      ("prop1.result", "prop2.result", "(prop1 || prop2).result", "msg"),
      (true, true, true, ""),
      (true, false, true, "bollocks"),
      (false, true, true, "bollocks"),
      (false, false, false, "bollocks\nbollocks")
    )
    forAll(tests)(testOr)
  }

  behavior of "8.10 Gen.unsized"
  it should "work" in {
    forAll(between0And100) { n =>
      assert("X" == Gen.unit("X").unsized.forSize(n).get)
    }
  }

  behavior of "8.11.1 SGen.apply"

  private def unitSGen: SGen[Int] = SGen(Gen.unit(_))

  it should "work" in {
    forAll(between0And100) { n =>
      assert(n == unitSGen(n).get)
    }
  }

  behavior of "8.11.2 SGen.map"
  it should "work" in {
    forAll(between0And100) { n =>
      assert(n + 1 == unitSGen(n).map(_ + 1).get)
    }
  }

  behavior of "8.11.3 SGen.flatMap"
  it should "work" in {
    forAll(between0And100) { n =>
      assert(n + 1 == unitSGen(n).flatMap(n => unitSGen(n + 1)).get)
    }
  }

  behavior of "8.11.4 SGen.**"
  it should "work" in {
    forAll(between0And100) { n =>
      assert((n, n + 1) == (unitSGen(n) ** unitSGen(n + 1)).get)
    }
  }

  behavior of "8.12 SGen.listOf"
  it should "work" in {
    forAll(between0And100) { n =>
      assert(List.fill(n)("X") == Gen.listOf(Gen.unit("X")).forSize(n).get)
    }
  }

  behavior of "8.13 SGen.listOf1"
  it should "work" in {
    forAll(between0And100) { n =>
      assert(List.fill(n max 1)("X") == Gen.listOf1(Gen.unit("X")).forSize(n).get)
    }
  }

  behavior of "8.14 List.sorted Props"
  it should "work" in {
    val result = ListProps.sortedProp.run(10, rng)
    assert(result == Passed)
  }

  behavior of "8.16 Richer Generator for Par[Int]"
  it should "work" in {
    import java.util.concurrent._
    import java.util.concurrent.atomic.AtomicInteger

    val asyncThreadCount = new AtomicInteger(0)
    val threadFactory: ThreadFactory =
      new ThreadFactory {
        override def newThread(r: Runnable) = {
          asyncThreadCount.incrementAndGet
          Executors.defaultThreadFactory.newThread(r)
        }
      }
    val executorService: ExecutorService = Executors.newCachedThreadPool(threadFactory)
    val SampleCount = 100
    try {
      val genFutureInt: Gen[Future[Int]] = Gen.parInt map (Par.run(executorService)(_))
      val genList = Gen.listOfN(SampleCount, genFutureInt)
      val (futureInts, _) = genList.sample.run(RNG.Simple(42))
      val ints = futureInts map (_.get)
      assert(asyncThreadCount.get / SampleCount.toDouble > 2)
    } finally executorService.shutdown
  }

  behavior of "8.18 List.takeWhile Props"
  it should "work" in {
    val result = ListProps.takeWhileProp.run(10, rng)
    assert(result == Passed)
  }
}

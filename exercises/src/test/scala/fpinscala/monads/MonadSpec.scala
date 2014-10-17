package fpinscala.monads

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import Monad.idMonad
import Monad.listMonad
import Monad.optionMonad
import Monad.parMonad
import Monad.parserMonad
import Monad.readerMonad
import Monad.stateMonad
import Monad.streamMonad
import fpinscala.parallelism.Par.Par
import fpinscala.parallelism.Par.{run => prun}
import fpinscala.parsing.ParserImpl
import fpinscala.parsing.ParserTypes
import fpinscala.state.State

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MonadSpec extends FlatSpec with PropertyChecks with BeforeAndAfterEach {

  // tests w/ Int are simplest
  private type T = Int

//  private implicit def arbitraryMonad[M[_] <: Monad[M]](m: M[T]): Arbitrary[M[T]] =
//    Arbitrary(Gen.choose(-100, 100) map(m.unit(_)))

  var executorService: ExecutorService = _

  override def beforeEach = executorService = Executors.newCachedThreadPool
  override def afterEach = executorService.shutdown

  private[MonadSpec] case class MonadTest[F[_]](M: Monad[F],
      mEq: (F[Int], F[Int]) => Boolean = ((_:F[Int]) == (_:F[Int])))
  {
    import M._

    private implicit def arbitraryM: Arbitrary[F[T]] =
      Arbitrary(arbitrary[T] map (unit(_)))

    def kleisli[B](f: T => B) = (a: T) => unit[B](f(a))
    val f = kleisli[T](_ + 1)
    val g = kleisli(_ + 2)
    val h = kleisli(_ + 4)
    val fg = kleisli(_ + 3)
    val fgh = kleisli(_ + 7)

    def testMonad = {
      testFlatMap
      testUnit
      mapPreservesStructure
    }

    def testFlatMap = {
      forAll("n") { n: T =>
        assertEq(flatMap(f(n))(g), fg(n))
      }
      forAll("m") { m: F[T] =>
        assertEq(flatMap(m)(unit(_)), m)
      }
    }
    def testUnit =
      forAll("n") { n: T =>
        assertEq(flatMap(unit(n))(unit(_)), unit(n))
      }
    def mapPreservesStructure =
      forAll("n") { m: F[T] =>
        assertEq(map(m)(identity[T]), m)
      }
    private def assertEq(m1: F[T], m2: F[T]) =
      assert(mEq(m1, m2), s"""eq($m1, $m2)""")

    def testSequence =
      forAll("l") { l: List[T] =>
        val lma = l map(unit(_))
        assert(sequence(lma) == unit(l))
      }

    def testTraverse =
      forAll("l") { l: List[T] =>
        assert(traverse(l)(f) == unit(l map(_ + 1)))
      }

    def testReplicateM =
      forAll(Gen.choose(0, 100) label "n") { n: Int =>
        assert(replicateM(n, unit(1)) == unit(List.fill(n)(1)))
      }

    def testCompose =
      forAll("n") { n: T =>
        assert(compose(f, g)(n) == fg(n))
        assert(compose(compose(f, g), h)(n) == fgh(n))
      }

    def associativeLaw =
      forAll("n") { n: T =>
        assert(compose(compose(f, g), h)(n) == compose(f, compose(g, h))(n))
      }

    def identityLaws = {
      val strictUnit = (t: T) => unit(t)
      val leftIdentity = compose(f, strictUnit)
      val rightIdentity = compose(unit[T], f)

      forAll("n") { n: T =>
        assert(leftIdentity(n) == f(n), "leftIdentity")
        assert(rightIdentity(n) == f(n), "rightIdentity")
        assert(flatMap(f(n))(strictUnit) == f(n), "leftIdentity (flatMap)")
        assert(flatMap(strictUnit(n))(f) == f(n), "rightIdentity (flatMap)")
      }
    }
  }

  lazy val listMonadTest = MonadTest(listMonad)
  lazy val optionMonadTest = MonadTest(optionMonad)
  lazy val streamMonadTest = MonadTest(streamMonad)
  lazy val parMonadTest = {
    import fpinscala.parallelism.Par.{run => prun, Par}
    MonadTest(parMonad,
        (p1: Par[Int], p2: Par[Int]) => prun(executorService)(p1) == prun(executorService)(p2))
  }

  lazy val parserMonadTest =
    MonadTest(parserMonad(ParserImpl),
      (p1: ParserTypes.Parser[Int], p2: ParserTypes.Parser[Int]) => {
        ParserImpl.run(p1)("") == ParserImpl.run(p1)("")
      })

  lazy val idMonadTest = MonadTest(idMonad)

  behavior of "11.1.1 parMonad"
  it should "work" in  parMonadTest.testMonad

  behavior of "11.1.2 ParserMonad"
  it should "work" in parserMonadTest.testMonad

  behavior of "11.1.3 optionMonad"
  it should "work" in optionMonadTest.testMonad

  behavior of "11.1.4 streamMonad"
  it should "work" in streamMonadTest.testMonad

  behavior of "11.1.5 listMonad"
  it should "work" in listMonadTest.testMonad

  behavior of "11.3.1 sequence"
  it should "work in ListMonad" in listMonadTest.testSequence
  it should "work in OptionMonad" in optionMonadTest.testSequence

  behavior of "11.3.2 traverse"
  it should "work in ListMonad" in listMonadTest.testTraverse
  it should "work in OptionMonad" in optionMonadTest.testTraverse

  behavior of "11.4 replicateM"
  it should "work in ListMonad" in listMonadTest.testReplicateM
  it should "work in OptionMonad" in optionMonadTest.testReplicateM

  behavior of "11.5.1 replicateM in ListMonad"
  it should "work" in {
    val tests =
      Table(
        ("n", "ma: List[Int]", "replicateM(n, ma)"),
        (0, List[Int](), List(List[Int]())),
        (1, List[Int](), List()),
        (2, List[Int](), List()),
        (3, List[Int](), List()),

        (0, List(1), List(List[Int]())),
        (1, List(1), List(List(1))),
        (2, List(1), List(List(1, 1))),
        (3, List(1), List(List(1, 1, 1))),

        (0, List(1, 2), List(List[Int]())),
        (1, List(1, 2), List(List(1), List(2))),
        (2, List(1, 2), List(List(1, 1), List(1, 2), List(2, 1), List(2, 2))),
        (3, List(1, 2), List(List(1, 1, 1), List(1, 1, 2), List(1, 2, 1), List(1, 2, 2), List(2, 1, 1),
          List(2, 1, 2), List(2, 2, 1), List(2, 2, 2))))
    forAll(tests) { (n: Int, ma: List[Int], expected: List[List[Int]]) =>
      assert(listMonad.replicateM(n, ma) == expected)
    }
  }

  behavior of "11.5.2 replicateM in OptionMonad"
  it should "work" in {
    val tests =
      Table[Int, Option[Int], Option[List[Int]]](
        ("n", "ma: Option[Int]", "replicateM(n, ma)"),
        (0, None, Some(List[Int]())),
        (1, None, None),
        (2, None, None),
        (3, None, None),

        (0, Some(1), Some(List())),
        (1, Some(1), Some(List(1))),
        (2, Some(1), Some(List(1, 1))),
        (3, Some(1), Some(List(1, 1, 1))))
    forAll(tests) { (n: Int, ma: Option[Int], expected: Option[List[Int]]) =>
      assert(optionMonad.replicateM(n, ma) == expected)
    }
  }

  behavior of "11.6.1 filterM in ListMonad"
  it should "work" in {
    def evenList(i: Int): List[Boolean] = List(i % 2 == 0)
    val tests =
      Table(
        ("la: List[Int]", "filterM(ma)(evenList"),
        (List[Int](), List(List[Int]())),
        (List(1, 3), List(List[Int]())),
        (List(2, 4), List(List(2, 4))),
        (List(1, 2, 3, 4), List(List(2, 4))))
    forAll(tests) { (la: List[Int], expected: List[List[Int]]) =>
      assert(listMonad.filterM(la)(evenList) == expected)
    }
  }

  behavior of "11.6.2 filterM in OptionMonad"
  it should "work" in {
    def evenOption(i: Int): Option[Boolean] = Some(i % 2 == 0)
    val tests =
      Table(
        ("la: List[Int]", "filterM(ma)(evenOption"),
        (List[Int](), Some(List[Int]())),
        (List(1, 3), Some(List[Int]())),
        (List(2, 4), Some(List(2, 4))),
        (List(1, 2, 3, 4), Some(List(2, 4))))
    forAll(tests) { (la: List[Int], expected: Some[List[Int]]) =>
      assert(optionMonad.filterM(la)(evenOption) == expected)
    }
  }

  behavior of "11.7 compose"

  it should "work in ListMonad" in listMonadTest.testCompose
  it should "obey the associative law in ListMonad" in listMonadTest.associativeLaw

  it should "work in OptionMonad" in optionMonadTest.testCompose
  it should "obey the associative law in OptionMonad" in optionMonadTest.associativeLaw

  behavior of "11.8 flatMapViaCompose"
  it should "work in ListMonad" in {
    import listMonadTest._
    import listMonadTest.M._
    forAll("n") { n: T =>
      assert(flatMapViaCompose(f(n))(g) == fg(n))
      assert(flatMapViaCompose(f(n))(g) == flatMap(f(n))(g))
    }
  }
  it should "work in OptionMonad" in {
    import optionMonadTest._
    import optionMonadTest.M._
    forAll("n") { n: T =>
      assert(flatMapViaCompose(f(n))(g) == fg(n))
      assert(flatMapViaCompose(f(n))(g) == flatMap(f(n))(g))
    }
  }

  behavior of "identity laws"
  it should "work in ListMonad" in listMonadTest.identityLaws
  it should "work in OptionMonad" in optionMonadTest.identityLaws

  behavior of "11.12 join"
  it should "work in ListMonad" in {
    import listMonadTest._
    import listMonadTest.M._
    forAll("mma") { mma: List[List[T]] =>
      assert(join(mma) == mma.flatten)
    }
  }
  it should "work in OptionMonad" in {
    import optionMonadTest._
    import optionMonadTest.M._
    forAll("mma") { mma: Option[Option[T]] =>
      assert(join(mma) == mma.flatten)
    }
  }

  behavior of "11.13.1 flatMapViaJoinAndMap"
  it should "work in ListMonad" in {
    import listMonadTest._
    import listMonadTest.M._
    forAll("n") { n: T =>
      assert(flatMapViaJoinAndMap(f(n))(g) == fg(n))
      assert(flatMapViaJoinAndMap(f(n))(g) == flatMap(f(n))(g))
    }
  }
  it should "work in OptionMonad" in {
    import optionMonadTest._
    import optionMonadTest.M._
    forAll("n") { n: T =>
      assert(flatMapViaJoinAndMap(f(n))(g) == fg(n))
      assert(flatMapViaJoinAndMap(f(n))(g) == flatMap(f(n))(g))
    }
  }

  behavior of "11.13.2 composeViaJoinAndMap"
  it should "work in ListMonad" in {
    import listMonadTest._
    import listMonadTest.M._
    forAll("n") { n: T =>
      assert(composeViaJoinAndMap(f, g)(n) == fg(n))
      assert(composeViaJoinAndMap(composeViaJoinAndMap(f, g), h)(n) == fgh(n))
    }
  }
  it should "work in OptionMonad" in {
    import optionMonadTest._
    import optionMonadTest.M._
    forAll("n") { n: T =>
      assert(composeViaJoinAndMap(f, g)(n) == fg(n))
      assert(composeViaJoinAndMap(composeViaJoinAndMap(f, g), h)(n) == fgh(n))
    }
  }

  behavior of "11.17 idMonad"
  it should "work" in idMonadTest.testMonad

  lazy val intStateMonad = stateMonad[T]

  behavior of "11.18.1 stateMonad.replicateM"
  it should "work" in {
    import intStateMonad._
    type S = Int
    type A = Int
    val a: A = 1
    val ma: State[S,A] = unit(a)
    val s0: S = 0
    val tests = Table(
      ("n", "replicateM(n, unit(a))"),
      (0, unit(List())),
      (1, unit(List(a))),
      (2, unit(List(a, a))),
      (3, unit(List(a, a, a))))
    forAll(tests) { (n: Int, expected: State[S,List[A]]) =>
      assert(replicateM(n, ma).run(s0) == expected.run(s0))
    }
  }

  behavior of "11.18.2 stateMonad.map2"
  it should "work" in {
    import intStateMonad._
    type S = Int
    type A = Int
    type B = Int
    type C = Int
    val s0: S = 0
    val f: (A,B) => C  = _ + _
    val tests = Table(
      ("ma: State[S,A]", "mb: State[S,B]", "map2(ma, mb)(_ + _)"),
      (unit(1), unit(2), unit(f(1, 2))),
      (unit(2), unit(3), unit(f(2, 3))))
    forAll(tests) { (ma: State[S,A], mb: State[S,B], expected: State[S,A]) =>
      assert(map2(ma, mb)(f).run(s0) == expected.run(s0))
    }
  }

  behavior of "11.18.3 stateMonad.sequence"
  it should "work" in {
    import intStateMonad._
    type S = Int
    type A = Int
    val s0: S = 0
    val tests = Table(
      ("lma: List[State[S,A]]", "sequence(lma)"),
      (List(), unit(List())),
      (List(unit(1)), unit(List(1))),
      (List(unit(1), unit(2)), unit(List(1,2))))
    forAll(tests) { (lma: List[State[S,A]], expected: State[S,List[A]]) =>
      assert(sequence(lma).run(s0) == expected.run(s0))
    }
  }

  behavior of "11.19 stateMonad laws"
  it should "hold" in {
    import intStateMonad._
    import Monad._
    type S = Int

    stateEq(getState[S], getState[S].flatMap(unit(_)))

    def stateEq(s1: State[S,S], s2: State[S,S]) =
      forAll("s") { s: S =>
        assert(s1.run(s) == s2.run(s), s"s1.run($s) == s2.run($s)")
      }
  }

  behavior of "11.20 readerMonad"
  it should "work" in {
    val rm = readerMonad[T]
    import rm._
    forAll("a") { a: T =>
      assert(unit(a).run(-1) == a)
    }
  }
}
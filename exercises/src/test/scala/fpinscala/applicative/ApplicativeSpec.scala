package fpinscala.applicative

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen


@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ApplicativeSpec extends FlatSpec with PropertyChecks {

  // tests w/ Int are simplest
  private type T = Int

//  private implicit def arbitraryApplicative[M[_] <: Applicative[M]](m: M[T]): Arbitrary[M[T]] =
//    Arbitrary(Gen.choose(-100, 100) map(m.unit(_)))

  private[ApplicativeSpec] case class ApplicativeTest[F[_]](M: Applicative[F],
      mEq: (F[Int], F[Int]) => Boolean = ((_:F[Int]) == (_:F[Int])))
{
    import M._

    private implicit def arbitraryA: Arbitrary[F[T]] =
      Arbitrary(arbitrary[T] map (unit(_)))

    private def assertEq(m1: F[T], m2: F[T]) =
      assert(mEq(m1, m2), s"""eq($m1, $m2)""")

    def testSequence =
      forAll("l") { l: List[T] =>
        val lma = l map(unit(_))
        assert(sequence(lma) == unit(l))
      }

    def testTraverse = {
      val f = (a:T) => unit(a + 1)
      forAll("l") { l: List[T] =>
        assert(traverse(l)(f) == unit(l map(_ + 1)))
      }
    }

    def testReplicateM =
      forAll(Gen.choose(0, 100) label "n") { n: Int =>
        assert(replicateM(n, unit(1)) == unit(List.fill(n)(1)))
      }

    def testProduct =
      forAll("a", "b") { (a: T, b: T) =>
        assert(product(unit(a), unit(b)) == unit((a,b)))
      }

    def mapPreservesStructure =
      forAll("n") { m: F[T] =>
        assertEq(map(m)(identity[T]), m)
      }

    def testMap3 = {
      val f = (a:T, b:T, c:T) => a + b + c
      forAll("a", "b", "c") { (a: T, b: T, c: T) =>
        assert(map3(unit(a), unit(b), unit(c))(f) == unit(f(a,b,c)))
      }
    }

    def testMap4 = {
      val f = (a:T, b:T, c:T, d:T) => a + b + c + d
      forAll("a", "b", "c", "d") { (a: T, b: T, c: T, d:T) =>
        assert(map4(unit(a), unit(b), unit(c), unit(d))(f) == unit(f(a,b,c,d)))
      }
    }
  }

  import Applicative._
  lazy val listApplicativeTest = ApplicativeTest(listApplicative)
  lazy val optionApplicativeTest = ApplicativeTest(optionApplicative)

  behavior of "12.1.1 sequence"
  it should "work in ListApplicative" in listApplicativeTest.testSequence
  it should "work in OptionApplicative" in optionApplicativeTest.testSequence

  behavior of "12.1.2 replicateM"
  it should "work in ListApplicative" in listApplicativeTest.testReplicateM
  it should "work in OptionApplicative" in optionApplicativeTest.testReplicateM

  behavior of "12.1.3 product"
  it should "work in ListApplicative" in listApplicativeTest.testProduct
  it should "work in OptionApplicative" in optionApplicativeTest.testProduct

  behavior of "12.1.4 traverse"
  it should "work in ListApplicative" in listApplicativeTest.testTraverse
  it should "work in OptionApplicative" in optionApplicativeTest.testTraverse

  behavior of "12.2 map via unit and apply"
  it should "work in ListApplicative" in listApplicativeTest.mapPreservesStructure
  it should "work in OptionApplicative" in optionApplicativeTest.mapPreservesStructure

  behavior of "12.3.1 map3"
  it should "work in ListApplicative" in listApplicativeTest.testMap3
  it should "work in OptionApplicative" in optionApplicativeTest.testMap3

  behavior of "12.3.2 map4"
  it should "work in ListApplicative" in listApplicativeTest.testMap4
  it should "work in OptionApplicative" in optionApplicativeTest.testMap4

}
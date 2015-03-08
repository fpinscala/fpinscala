package fpinscala.applicative

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import java.util.Date
import scala.util.Try

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ApplicativeSpec extends FlatSpec with PropertyChecks {

  // tests w/ Int are simplest
  private type T = Int

  //  private implicit def arbitraryApplicative[M[_] <: Applicative[M]](m: M[T]): Arbitrary[M[T]] =
  //    Arbitrary(Gen.choose(-100, 100) map(m.unit(_)))

  private[ApplicativeSpec] case class ApplicativeTest[F[_]](M: Applicative[F],
    mEq: (F[Int], F[Int]) => Boolean = ((_: F[Int]) == (_: F[Int]))) {
    import M._

    private implicit def arbitraryA: Arbitrary[F[T]] =
      Arbitrary(arbitrary[T] map (unit(_)))

    private def assertEq(m1: F[T], m2: F[T]) =
      assert(mEq(m1, m2), s"""eq($m1, $m2)""")

    def testSequence =
      forAll("l") { l: List[T] =>
        val lma = l map (unit(_))
        assert(sequence(lma) == unit(l))
      }

    def testTraverse = {
      val f = (a: T) => unit(a + 1)
      forAll("l") { l: List[T] =>
        assert(traverse(l)(f) == unit(l map (_ + 1)))
      }
    }

    def testReplicateM =
      forAll(Gen.choose(0, 100) label "n") { n: Int =>
        assert(replicateM(n, unit(1)) == unit(List.fill(n)(1)))
      }

    def testProduct =
      forAll("a", "b") { (a: T, b: T) =>
        assert(product(unit(a), unit(b)) == unit((a, b)))
      }

    def testMap3 = {
      val f = (a: T, b: T, c: T) => a + b + c
      forAll("a", "b", "c") { (a: T, b: T, c: T) =>
        assert(map3(unit(a), unit(b), unit(c))(f) == unit(f(a, b, c)))
      }
    }

    def testMap4 = {
      val f = (a: T, b: T, c: T, d: T) => a + b + c + d
      forAll("a", "b", "c", "d") { (a: T, b: T, c: T, d: T) =>
        assert(map4(unit(a), unit(b), unit(c), unit(d))(f) == unit(f(a, b, c, d)))
      }
    }

    def applicativeLaws = {
      functorLaws
      identityLaws
      associativeLaw
      naturalityLaw
    }

    def functorLaws = { // 214
      // map "preserves structure"
      forAll("m") { m: F[T] =>
        assertEq(map(m)(identity[T]), m)
      }

      val f = (a: T) => a + 1
      val g = (a: T) => a + 2
      forAll("m") { m: F[T] =>
        assertEq(map(map(m)(g))(f), map(m)(f compose g))
      }
    }

    def identityLaws = { // 215
      // map2 "preserves structure"
      forAll("m") { m: F[T] =>
        assertEq(map2(unit(()), m)((_, a) => a), m)
        assertEq(map2(m, unit(()))((a, _) => a), m)
      }
    }

    def associativeLaw = { // 216
      def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) =
        p match { case (a, (b, c)) => ((a, b), c) }

      forAll("fa", "fb", "fc") { (fa: F[T], fb: F[T], fc: F[T]) =>
        assert(product(product(fa, fb), fc) == map(product(fa, product(fb, fc)))(assoc))
      }
    }

    def naturalityLaw = {
      def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) =
        (i, i2) => (f(i), g(i2))
      val f = (_: T) + 1
      val g = (_: T) + 2

      forAll("a", "b") { (a: F[T], b: F[T]) =>
        assert(map2(a, b)(productF(f, g)) == product(map(a)(f), map(b)(g)))
      }
    }

    def testSequenceMap = {
      //      def toFunction(m: Map[T,T]) = m.apply _
      forAll("ofa") { ofa: Map[T, F[T]] =>
        //        val ofa = ttMap mapValues(unit(_))
        val fMap = sequenceMap(ofa)
        val fGet: F[T => T] = fMap.map(m => m(_)) // Map as Function
        ofa.keySet foreach { k: T =>
          assert(unit(k).apply(fGet) == ofa(k))
        }
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
  it should "obey the Functor laws in ListApplicative" in listApplicativeTest.functorLaws
  it should "obey the Functor laws in OptionApplicative" in optionApplicativeTest.functorLaws

  behavior of "12.3.1 map3"
  it should "work in ListApplicative" in listApplicativeTest.testMap3
  it should "work in OptionApplicative" in optionApplicativeTest.testMap3

  behavior of "12.3.2 map4"
  it should "work in ListApplicative" in listApplicativeTest.testMap4
  it should "work in OptionApplicative" in optionApplicativeTest.testMap4

  behavior of "12.4 StreamApplicative.sequence"
  it should "work" in {
    import streamApplicative._
    val l = List(1, 2)
    val sl = sequence(l map (unit(_))) // sequence(List(unit(1), unit(2)))
    forAll(Gen.choose(0, 10) label "n") { n =>
      val takeN = sl.take(n).toList
      assert(takeN == List.fill(n)(l))
      assert(takeN == unit(l).take(n).toList)
    }
  }

  case class WebForm(name: String, birthdate: Date, phoneNumber: String)

  trait CheckResult[R[String, _]] {
    type Result[A] = R[String, A]
    def success[A](a: A): Result[A]
    def failure[A](s: String): Result[A]

    val NameErrorMsg = "Name cannot be empty"
    val BirthDateErrorMsg = "Birthdate must be in the form yyyy-MM-dd"
    val PhoneNumberErrorMsg = "Phone number must be 10 digits"

    val NameFailure = failure[String](NameErrorMsg)
    val BirthDateFailure = failure[Date](BirthDateErrorMsg)
    val PhoneNumberFailure = failure[String](PhoneNumberErrorMsg)

    val (goodName, badName) = ("Darth", "")
    val (goodBirthDate, badBirthDate) = ("1970-01-01", "what?")
    val (goodPhoneNumber, badPhoneNumber) = ("1234567890", "what?")

    def parseDate(date: String): Date =
      new java.text.SimpleDateFormat("yyyy-MM-dd").parse(date)
    val WebFormSuccess = success(WebForm(goodName, parseDate(goodBirthDate), goodPhoneNumber))

    def validName(name: String): Result[String] =
      if (name != "") success(name)
      else NameFailure
    def validBirthdate(birthdate: String): Result[Date] =
      Try {
        success(parseDate(birthdate))
      } getOrElse BirthDateFailure
    def validPhone(phoneNumber: String): Result[String] =
      if (phoneNumber.matches("[0-9]{10}"))
        success(phoneNumber)
      else PhoneNumberFailure

    def getResults(toWebForm: (String, String, String) => Result[WebForm]) =
      (toWebForm(goodName, goodBirthDate, goodPhoneNumber),
        toWebForm(badName, goodBirthDate, goodPhoneNumber),
        toWebForm(badName, badBirthDate, goodPhoneNumber),
        toWebForm(badName, badBirthDate, badPhoneNumber))
  }

  behavior of "12.5 eitherMonad.flatMap"
  it should "stop at first failure in WebForm example" in {
    val checkResultEither = new CheckResult[Either] {
      override def success[A](a: A) = Right(a)
      override def failure[A](s: String) = Left(s)
    }
    import checkResultEither._
    val eitherM = Monad.eitherMonad[String]
    implicit class EitherOps[A](either: Either[String, A]) {
      def flatMap[B](f: A => Either[String, B]) = eitherM.flatMap(either)(f)
      def map[B](f: A => B) = eitherM.map(either)(f)
    }

    def monadicWebFormViaFlatMap(name: String, birthDate: String, phoneNumber: String) =
      //      for {
      //        n <- validName(name)
      //        d <- validBirthdate(birthDate)
      //        p <- validPhone(phoneNumber)
      //      } yield WebForm(n,d,p)
      validName(name) flatMap (f1 =>
        validBirthdate(birthDate) flatMap (f2 =>
          validPhone(phoneNumber) map (f3 => WebForm(f1, f2, f3))))
    val flatMapResults = getResults(monadicWebFormViaFlatMap)
    assert(flatMapResults ==
      (WebFormSuccess, NameFailure, NameFailure, NameFailure)) // always only first failure

    def monadicWebFormViaMap3(name: String, birthDate: String, phoneNumber: String) =
      eitherM.map3(
        validName(name),
        validBirthdate(birthDate),
        validPhone(phoneNumber))(WebForm(_, _, _))
    val map3Results = getResults(monadicWebFormViaMap3)
    assert(map3Results ==
      (WebFormSuccess, NameFailure, NameFailure, NameFailure)) // again: always only first failure
  }

  behavior of "12.6 validationApplicative"
  it should "accumulate errors in WebForm example" in {
    val checkResultValidation = new CheckResult[Validation] {
      override def success[A](a: A) = Success(a)
      override def failure[A](s: String) = Failure(s, Vector())
    }
    import checkResultValidation._

    def applicativeWebFormViaMap3(name: String, birthDate: String, phoneNumber: String) =
      validationApplicative.map3(
        validName(name),
        validBirthdate(birthDate),
        validPhone(phoneNumber))(WebForm(_, _, _))
    val map3Results = getResults(applicativeWebFormViaMap3)
    assert(map3Results ==
      (WebFormSuccess,
        failures(NameErrorMsg),
        failures(NameErrorMsg, BirthDateErrorMsg),
        failures(NameErrorMsg, BirthDateErrorMsg, PhoneNumberErrorMsg)))

    def failures(errors: String*) = Failure(errors.head, errors.tail.toVector)
  }

  behavior of "p.214 ff. Functor Laws"
  it should "hold for ListApplicative" in listApplicativeTest.applicativeLaws
  it should "hold for OptionApplicative" in optionApplicativeTest.applicativeLaws

  behavior of "12.8 product of two applicative functors"
  it should "work" in {
    //    type ProductType[A[_],B[_]] = ({ type f[x] = (A[x], B[x]) })#f
    val listOptionProduct = listApplicative.product(optionApplicative)
    assert(listOptionProduct.unit(1) == (List(1), (Some(1))))
    val listOptionProductTest =
      ApplicativeTest[({ type f[x] = (List[x], Option[x]) })#f](listOptionProduct)
    listOptionProductTest.applicativeLaws

    val optionListProduct = optionApplicative.product(listApplicative)
    assert(optionListProduct.unit(1) == (Some(1), (List(1))))
    val optionListProductTest =
      ApplicativeTest[({ type f[x] = (Option[x], List[x]) })#f](optionListProduct)
    optionListProductTest.applicativeLaws
  }

  behavior of "12.9 compose of two applicative functors"
  it should "work" in {
    val listOptionCompose = listApplicative.compose(optionApplicative)
    assert(listOptionCompose.unit(1) == List(Some(1)))
    val listOptionComposeTest =
      ApplicativeTest[({ type f[x] = List[Option[x]] })#f](listOptionCompose)
    listOptionComposeTest.applicativeLaws

    val optionListCompose = optionApplicative.compose(listApplicative)
    assert(optionListCompose.unit(1) == Some(List(1)))
    val optionListComposeTest =
      ApplicativeTest[({ type f[x] = Option[List[x]] })#f](optionListCompose)
    optionListComposeTest.applicativeLaws
  }

  behavior of "12.12 sequenceMap"
  it should "work in ListApplicative" in listApplicativeTest.testSequenceMap
  it should "work in OptionApplicative" in optionApplicativeTest.testSequenceMap

  import Traverse._

  behavior of "12.13.1 listTraverse"
  it should "behave as described on page 219" in {
    implicit val oa = optionApplicative
    forAll("fma") { fma: List[Option[T]] =>
      val expected = if (fma.contains(None)) None else Some(fma map (_.get))
      assert(listTraverse.sequence(fma) == expected)
    }
  }

  behavior of "12.13.2 optionTraverse"
  it should "behave as described on page 219" in {
    implicit val la = listApplicative
    forAll("fma") { fma: Option[List[T]] =>
      val expected =
        if (fma.isEmpty) List(None)
        else {
          val l = fma.get
          if (l.isEmpty) List() else List(Some(l.head))
        }
      assert(optionTraverse.sequence(fma) == expected)
    }
  }

  private implicit def arbTree[T](implicit ev: Arbitrary[T]): Arbitrary[Tree[T]] = {
    val MaxTreeDepth = 5 // to prevent StackOverflows
    def arbitraryTree(maxDepth: Int): Gen[Tree[T]] =
      for {
        h <- arbitrary[T]
        numChildren <- Gen.choose(0, maxDepth)
      } yield Tree(h, List.fill(numChildren)(arbitraryTree(maxDepth - 1).sample.get))
    Arbitrary(arbitraryTree(MaxTreeDepth))
  }

  behavior of "12.13.3 treeTraverse"
  it should "behave as described on page 219" in {
    def contains[A](ta: Tree[A], a: A): Boolean =
      ta.head == a || ta.tail.exists(contains(_, a))
    implicit val oa = optionApplicative
    forAll("toa") { toa: Tree[Option[T]] =>
      val expected = if (contains(toa, None)) None else Some(treeTraverse.map(toa)(_.get))
      assert(treeTraverse.sequence(toa) == expected)
    }
  }

  private[ApplicativeSpec] case class TraverseTest[F[_]](M: Traverse[F]) {
    import M._

    def testMap[B](f: T => B)(mf: F[T] => F[B])(implicit ev: Arbitrary[F[T]]) =
      forAll("tt") { tt: F[T] =>
        assert(map(tt)(_.toString) == mf(tt))
      }

    def testReverse[B](mf: F[T] => F[T])(implicit ev: Arbitrary[F[T]]) =
      forAll("tt") { tt: F[T] =>
        assert(reverse(tt) == mf(tt))
      }

    def testFoldLeft(f: (Int,T) => Int)(sum: F[T] => Int)(implicit ev: Arbitrary[F[T]]) =
      forAll("tt") { tt: F[T] =>
        assert(foldLeft(tt)(0)(f) == sum(tt))
      }

    implicit lazy val la = listApplicative
    implicit lazy val lo = optionApplicative
    def testFuse(expected: F[T] => (List[F[String]], Option[F[String]]))(implicit ev: Arbitrary[F[T]]) =
      forAll("tt") { tt: F[T] =>
        assert(fuse[List,Option,T,String](tt)(a => List(a.toString), a => Option(a.toString)) ==
          expected(tt))
      }
  }

  private lazy val listTraverseTest = new TraverseTest(listTraverse)
  private lazy val optionTraverseTest = new TraverseTest(optionTraverse)
  private lazy val treeTraverseTest = new TraverseTest(treeTraverse)

  behavior of "12.14 Traverse.map via traverse"
  it should "work for listTraverse" in listTraverseTest.testMap(_.toString)(_.map(_.toString))
  it should "work for optionTraverse" in optionTraverseTest.testMap(_.toString)(_.map(_.toString))
  it should "work for treeTraverse" in {
    def mapTree[A, B](tt: Tree[A])(f: A => B): Tree[B] =
      Tree(f(tt.head), tt.tail.map(mapTree(_)(f)))
    treeTraverseTest.testMap(_.toString)(mapTree(_)(_.toString))
  }

  behavior of "12.16 Traverse.reverse"
  it should "work for listTraverse" in listTraverseTest.testReverse(_.reverse)
  it should "work for optionTraverse" in optionTraverseTest.testReverse(identity[Option[T]])
  it should "obey the law on page 223 for listTraverse" in {
      import listTraverse._
      type F[T] = List[T]
      forAll("x", "y") { (x: F[T], y: F[T]) =>
        assert(
          toList(reverse(x)) ++ toList(reverse(y)) ==
          reverse(toList(y) ++ toList(x)))
      }
    }

  behavior of "12.17 Traverse.foldLeft via mapAccum"
  it should "work for listTraverse" in listTraverseTest.testFoldLeft(_ + _)(_.sum)
  it should "work for optionTraverse" in optionTraverseTest.testFoldLeft(_ + _)(_.getOrElse(0))
  it should "work for treeTraverse" in {
    def sumTree(tt: Tree[Int]): Int = tt.head + tt.tail.map(sumTree).sum
    treeTraverseTest.testFoldLeft(_ + _)(sumTree)
  }

  behavior of "12.18 Traverse.fuse"
  it should "work for listTraverse" in
    listTraverseTest.testFuse(lt => (List(lt map(_.toString)), Option(lt map(_.toString))))
  it should "work for optionTraverse" in
    optionTraverseTest.testFuse(lt => (List(lt map(_.toString)), Option(lt map(_.toString))))

  behavior of "12.19 Traverse.compose"
  it should "work for listTraverse" in
    forAll("lo") { lo: List[Option[T]] =>
      assert(listTraverse.compose(optionTraverse).map(lo)(identity) == lo)
    }
  it should "work for optionTraverse" in
    forAll("ol") { ol: Option[List[T]] =>
      assert(optionTraverse.compose(listTraverse).map(ol)(identity) == ol)
    }

  behavior of "12.20 Monad.composeM"

  private def listMonad: Monad[List] =
    new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def map[A,B](ma: List[A])(f: A => B): List[B] =
      ma map f
    override def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  private def optionMonad: Monad[Option] =
    new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)
    override def map[A,B](ma: Option[A])(f: A => B): Option[B] =
      ma map f
    override def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  it should "work for listTraverse" in {
    implicit lazy val lm = listMonad
    implicit lazy val om = optionMonad
    implicit lazy val lt = listTraverse
    implicit lazy val ot = optionTraverse
    forAll("lo") { lo: List[Option[T]] =>
      assert(Monad.composeM[List, Option].flatMap(lo)(x => List(Option(x))) == lo)
    }
  }
  it should "work for optionTraverse" in {
    implicit lazy val lm = listMonad
    implicit lazy val om = optionMonad
    implicit lazy val lt = listTraverse
    implicit lazy val ot = optionTraverse
    forAll("ol") { ol: Option[List[T]] =>
      assert(Monad.composeM[Option, List].flatMap(ol)(x => Option(List(x))) == ol)
    }
  }
}

package fpinscala.exercises.applicative

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Gen.{`**`, boolean as genBoolean, int as genInt}
import fpinscala.exercises.applicative.Applicative.Validated
import fpinscala.exercises.applicative.Applicative.Validated.*
import fpinscala.exercises.applicative.ApplicativeSuite.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.monads.Id
import munit.Assertions

import java.time.LocalDate
import java.time.format.DateTimeParseException

class ApplicativeSuite extends PropSuite:
  import ApplicativeSuite.{applicativeViaUnitAndApply as aplApply, applicativeViaUnitAndMap2 as aplMap}

  private val isEven: Int => Boolean = (i: Int) => i % 2 == 0

  test("Applicative.sequence")(genIntList) { intList =>
    val fas = intList.map(Id.apply)
    assertEquals(aplMap.sequence(fas), Id(intList))
  }

  test("Applicative.replicateM")(genShortNumber ** genString) { case i ** s =>
    assertEquals(aplMap.replicateM(i, Id(s)), Id(List.fill(i)(s)))
  }

  test("Applicative.product")(genInt ** genString) { case i ** s =>
    assertEquals(aplMap.product(Id(i))(Id(s)), Id((i, s)))
  }

  test("Applicative.apply")(genInt) { i =>
    val fb = aplMap.apply(Id(isEven))(Id(i))
    assertEquals(fb, Id(isEven(i)))
  }

  test("Applicative.map")(genInt) { i =>
    val fb = aplApply.map(Id(i))(isEven)
    assertEquals(fb, Id(isEven(i)))
  }

  test("Applicative.map2")(genInt ** genString) { case i ** s =>
    val fc = aplApply.map2(Id(i))(Id(s))((i, s) => (i, s))
    assertEquals(fc, Id((i, s)))
  }

  test("Applicative.map3")(genInt ** genString ** genChar) { case i ** s ** c =>
    val fd = aplApply.map3(Id(i))(Id(s), Id(c))((i, s, c) => (i, s, c))
    assertEquals(fd, Id((i, s, c)))
  }

  test("Applicative.map4")(genInt ** genString ** genChar ** genBoolean) { case i ** s ** c ** b =>
    val fe = aplApply.map4(Id(i))(Id(s), Id(c), Id(b))((i, s, c, b) => (i, s, c, b))
    assertEquals(fe, Id((i, s, c, b)))
  }

  test("Applicative.eitherMonad")(genInt ** genString) { case n ** s =>
    assertMonad[Either[Unit, _]](eitherMonad, n, s)
  }

  test("Validated.validatedApplicative")(genWebForm) { case ((name, nameErr), (date, dateErr), (phone, phoneErr)) =>
    val validated: Validated[List[String], WebForm] = validateWebForm(name, date, phone)
    val errors = List(phoneErr, nameErr, dateErr).flatten
    if errors.isEmpty then assertEquals(validated, Valid(WebForm("Bob", LocalDate.of(1984, 12, 31), "0123456789")))
    else assertEquals(validated, Invalid(errors))
  }

  test("Applicative laws")(genIntList) { intList =>
    assertApplicativeLaws[Option[_]](applOpt, intList, _.sum, _.toString)
  }

  test("Applicative.product (F[x], G[x])")(genIntList ** genInt) { case intList ** n =>
    val product = applOpt.product(aplApply)
    assertEquals(product.unit(n), (Some(n), Id(n)))
    assertApplicativeLaws[[x] =>> (Option[x], Id[x])](product, intList, _.sum, _.toString)
  }

  test("Applicative.compose")(genIntList ** genInt) { case intList ** n =>
    val compose = applOpt.compose(aplApply)
    assertEquals(compose.unit(n), Some(Id(n)))
    assertApplicativeLaws[[x] =>> Option[Id[x]]](compose, intList, _.sum, _.toString)
  }

  test("Applicative.sequenceMap")(Gen.unit(())) { _ =>
    val empty = applOpt.sequenceMap[String, Int](Map.empty[String, Option[Int]])
    assertEquals(empty, Some(Map.empty[String, Int]))

    val none = applOpt.sequenceMap[String, Int](Map("key" -> None))
    assertEquals(none, None)

    val containsNone = applOpt.sequenceMap[String, Int](Map("key1" -> Some(1), "key2" -> None))
    assertEquals(containsNone, None)

    val onlySome = applOpt.sequenceMap[String, Int](Map("key1" -> Some(1), "key2" -> Some(2)))
    assertEquals(onlySome, Some(Map("key1" -> 1, "key2" -> 2)))
  }

object ApplicativeSuite extends Assertions:
  given applicativeViaUnitAndMap2: Applicative[Id[_]] with
    def unit[A](a: => A): Id[A] = Id(a)
    extension [A](fa: Id[A])
      override def map2[B, C](fb: Id[B])(f: (A, B) => C): Id[C] =
        Id(f(fa.value, fb.value))

  given applicativeViaUnitAndApply: Applicative[Id[_]] with
    def unit[A](a: => A): Id[A] = Id(a)
    override def apply[A, B](fab: Id[A => B])(fa: Id[A]): Id[B] =
      Id(fab.value(fa.value))

  val applOpt: Applicative[Option[_]] = new:
    def unit[A](a: => A): Option[A] = Some(a)
    override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
      for
        f <- fab
        a <- fa
      yield f(a)

  trait TestedMonad[F[_]]:
    val monad: Monad[F]
    def pure[A]: A => F[A]
    def assertFs[A](actual: F[A], expected: F[A]): Unit =
      assertEquals(actual, expected)
    def f: List[Int] => F[Int] = list => pure(list.sum)
    def g: Int => F[String] = i => pure(i.toString)
    def h: String => F[Boolean] = s => pure(s.startsWith("-"))

  private val eitherMonad: TestedMonad[Either[Unit, _]] =
    new TestedMonad[Either[Unit, _]]:
      val monad: Monad[Either[Unit, _]] = Applicative.eitherMonad[Unit]
      def pure[A]: A => Either[Unit, A] = Right(_)

  def assertMonad[F[_]](tm: TestedMonad[F[_]], n: Int, s: String): Unit =
    assertUnit(tm, n)
    assertFlatMap(tm, n)
    assertMap(tm, n)
    assertMap2(tm, n, s)

  private def assertUnit[F[_]](tm: TestedMonad[F[_]], n: Int): Unit =
    import tm.*
    assertFs(monad.unit(n), pure(n))

  private def assertFlatMap[F[_]](tm: TestedMonad[F[_]], n: Int): Unit =
    import tm.*
    val appliedFlatMap = monad.flatMap(monad.unit(n))(i => monad.unit(i % 2 == 0))
    assertFs(appliedFlatMap, pure(n % 2 == 0))

  private def assertMap[F[_]](tm: TestedMonad[F[_]], n: Int): Unit =
    import tm.*
    val appliedMap = monad.map(monad.unit(n))(_ % 2 == 0)
    assertFs(appliedMap, pure(n % 2 == 0))

  private def assertMap2[F[_]](tm: TestedMonad[F[_]], n: Int, s: String): Unit =
    import tm.*
    val appliedMap2 = monad.map2(monad.unit(n))(monad.unit(s))((_, _))
    assertFs(appliedMap2, pure((n, s)))

  private case class WebForm(name: String, birthdate: LocalDate, phoneNumber: String)

  private val genWebForm =
    for
      name <- Gen.union(Gen.unit(("", Some("Name cannot be empty"))), Gen.unit(("Bob", None)))
      date <- Gen.union(
        Gen.unit(("31-12-1984", Some("Birthdate must be in the form yyyy-MM-dd"))),
        Gen.unit(("1984-12-31", None))
      )
      phone <- Gen.union(
        Gen.unit(("123456789", Some("Phone number must be 10 digits"))),
        Gen.unit(("0123456789", None))
      )
    yield (name, date, phone)

  private def validName(name: String): Validated[List[String], String] =
    if name != "" then Valid(name)
    else Invalid(List("Name cannot be empty"))

  private def validBirthdate(birthdate: String): Validated[List[String], LocalDate] =
    try Valid(LocalDate.parse(birthdate))
    catch case _: DateTimeParseException => Invalid(List("Birthdate must be in the form yyyy-MM-dd"))

  private def validPhone(phoneNumber: String): Validated[List[String], String] =
    if phoneNumber.matches("[0-9]{10}") then Valid(phoneNumber)
    else Invalid(List("Phone number must be 10 digits"))

  private def validateWebForm(
                               name: String,
                               birthdate: String,
                               phone: String
                             ): Validated[List[String], WebForm] =
    validName(name).map3(
      validBirthdate(birthdate),
      validPhone(phone)
    )(WebForm(_, _, _))

  private def assertApplicativeLaws[F[_]](
                                           applicative: Applicative[F[_]],
                                           a: List[Int],
                                           f: List[Int] => Int,
                                           g: Int => String
                                         ): Unit =
    import applicative.unit

    val fa = unit(a)
    val fb = unit(f(a))
    val fc = unit(g(f(a)))
    assertFunctorLaws[F[_]](applicative, a, f, g)
    assertTheLeftAndRightIdentityLaws[F[_]](applicative, a)
    assertAssociativeLaw[F[_]](applicative, fa, fb, fc)
    assertNaturalityLaw[F[_]](applicative, fa, fb, f, g)

  private def assertFunctorLaws[F[_]](
                                       appl: Applicative[F[_]],
                                       a: List[Int],
                                       f: List[Int] => Int,
                                       g: Int => String
                                     ): Unit =
    import appl.{map, unit}

    val v: F[List[Int]] = unit(a)
    assertEquals(map(v)(identity), v)
    assertEquals(map(map(v)(f))(g), map(v)(g compose f))

  private def assertTheLeftAndRightIdentityLaws[F[_]](appl: Applicative[F[_]], a: List[Int]): Unit =
    import appl.{map2, unit}

    val fa: F[List[Int]] = unit(a)
    assertEquals(map2(unit(()))(fa)((_, a) => a), fa)
    assertEquals(map2(fa)(unit(()))((a, _) => a), fa)

  private def assertAssociativeLaw[F[_]](appl: Applicative[F[_]], fa: F[List[Int]], fb: F[Int], fc: F[String]): Unit =
    import appl.{map, product}

    def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) = p match { case (a, (b, c)) => ((a, b), c) }

    val leftSide: F[((List[Int], Int), String)] = product(product(fa)(fb))(fc)
    val rightSide: F[(List[Int], (Int, String))] = product(fa)(product(fb)(fc))
    assertEquals(leftSide, map(rightSide)(assoc))

  private def assertNaturalityLaw[F[_]](
                                         appl: Applicative[F[_]],
                                         fa: F[List[Int]],
                                         fb: F[Int],
                                         a2c: List[Int] => Int,
                                         b2d: Int => String
                                       ): Unit =
    import appl.{map, map2, product}

    def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) = (i, i2) => (f(i), g(i2))

    assertEquals(map2(fa)(fb)(productF(a2c, b2d)), product(map(fa)(a2c))(map(fb)(b2d)))

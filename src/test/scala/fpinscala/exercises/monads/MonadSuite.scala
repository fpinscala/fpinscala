package fpinscala.exercises.monads

import fpinscala.answers.testing.exhaustive.Gen.{`**`, int as genInt}
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.monads.MonadSuite.*
import fpinscala.exercises.parallelism.Par
import fpinscala.exercises.parallelism.Par.Par
import fpinscala.exercises.parsing.UnitTestParser
import fpinscala.exercises.parsing.UnitTestParser.{Parser, succeed}
import fpinscala.exercises.state.RNG
import fpinscala.exercises.testing.Gen
import munit.Assertions

import java.util.concurrent.*
import scala.collection.immutable.Iterable

class MonadSuite extends PropSuite:
  test("genMonad")(genInt ** genString ** genRNG) { case n ** s ** rng =>
    assertMonad[Gen](genMonad(rng), n, s)
  }

  test("parMonad")(genInt ** genString) { case n ** s =>
    assertMonad[Par](parMonad, n, s)
  }

  test("parserMonad")(genInt ** genString) { case n ** s =>
    assertMonad[Parser[_]](parserMonad(s), n, s)
  }

  test("optionMonad")(genInt ** genString) { case n ** s =>
    assertMonad[Option[_]](optionMonad, n, s)
  }

  test("lazyListMonad")(genInt ** genString) { case n ** s =>
    assertMonad[LazyList[_]](lazyListMonad, n, s)
  }

  test("listMonad")(genInt ** genString) { case n ** s =>
    assertMonad[List[_]](listMonad, n, s)
  }

  test("Monad.sequence")(genIntList ** genRNG) { case intList ** rng =>
    val tm = genMonad(rng)
    import tm.*
    val listMonad = monad.sequence(intList.map(Gen.unit))
    assertFs(listMonad, pure(intList))
  }

  test("Monad.traverse")(genIntList ** genRNG) { case intList ** rng =>
    val tm = genMonad(rng)
    import tm.*
    val listMonad = monad.traverse(intList)(Gen.unit)
    assertFs(listMonad, pure(intList))
  }

  // ToDo: Uncomment after fpinscala.exercises.testing.GenSuite passing
/*
  test("Monad.replicateM")(genShortNumber ** genString ** genRNG) { case n ** s ** rng =>
    val tm = genMonad(rng)
    import tm.monad
    val fa = Gen.choose(0, 1000)
    val listMonad = monad.replicateM(n, fa)
    val intList: List[Int] = listMonad.next(rng)._1
    assertEquals(intList.length, n)
    assert(intList.forall(i => 0 <= i && i <= 1000))
  }
*/

  test("Monad.filterM")(genIntList ** genRNG) { case intList ** rng =>
    val tm = genMonad(rng)
    import tm.*
    val filteredListM = monad.filterM(intList)(i => Gen.unit(i % 2 == 0))
    assertFs(filteredListM, pure(intList.filter(_ % 2 == 0)))
  }

  test("The associative law")(genIntList ** genString ** genRNG) { case intList ** s ** rng =>
    assertAssociativeLaw[Gen](genMonad(rng), intList)
    assertAssociativeLaw[Par](parMonad, intList)
    assertAssociativeLaw[Parser[_]](parserMonad(s), intList)
    assertAssociativeLaw[Option[_]](optionMonad, intList)
    assertAssociativeLaw[LazyList[_]](lazyListMonad, intList)
    assertAssociativeLaw[List[_]](listMonad, intList)
  }

  test("Monad.compose")(genIntList ** genString ** genRNG) { case intList ** s ** rng =>
    val tm = genMonad(rng)
    import tm.*
    val fThenG = monad.compose(f, g)
    assertFs(fThenG(intList), pure(intList.sum.toString))
  }

  test("Monad.compose should be associative")(genIntList ** genString ** genRNG) { case intList ** s ** rng =>
    assertAssociativeCompose[Gen](genMonad(rng), intList)
    assertAssociativeCompose[Par](parMonad, intList)
    assertAssociativeCompose[Parser[_]](parserMonad(s), intList)
    assertAssociativeCompose[Option[_]](optionMonad, intList)
    assertAssociativeCompose[LazyList[_]](lazyListMonad, intList)
    assertAssociativeCompose[List[_]](listMonad, intList)
  }

  test("Monad.flatMapViaCompose")(genInt ** genRNG) { case n ** rng =>
    val tm = genMonad(rng)
    import tm.*
    val mappedMonad = monad.flatMapViaCompose(pure(n))((i: Int) => pure(i % 2 == 0))
    assertFs(mappedMonad, pure(n % 2 == 0))
  }

  test("The identity law")(genIntList ** genString ** genRNG) { case intList ** s ** rng =>
    assertIdentityLawForCompose[Gen](genMonad(rng), intList)
    assertIdentityLawForCompose[Par](parMonad, intList)
    assertIdentityLawForCompose[Parser[_]](parserMonad(s), intList)
    assertIdentityLawForCompose[Option[_]](optionMonad, intList)
    assertIdentityLawForCompose[LazyList[_]](lazyListMonad, intList)
    assertIdentityLawForCompose[List[_]](listMonad, intList)

    assertIdentityLawForFlatMap[Gen](genMonad(rng), intList)
    assertIdentityLawForFlatMap[Par](parMonad, intList)
    assertIdentityLawForFlatMap[Parser[_]](parserMonad(s), intList)
    assertIdentityLawForFlatMap[Option[_]](optionMonad, intList)
    assertIdentityLawForFlatMap[LazyList[_]](lazyListMonad, intList)
    assertIdentityLawForFlatMap[List[_]](listMonad, intList)
  }

  test("Monad.join")(genInt ** genRNG) { case n ** rng =>
    val tm = genMonad(rng)
    import tm.*
    val ffa = monad.join(Gen.unit(Gen.unit(n)))
    val fa = Gen.unit(n)
    assertFs(ffa, fa)
  }

  test("Monad.flatMapViaJoinAndMap")(genInt ** genRNG) { case n ** rng =>
    val tm = genMonad(rng)
    import tm.*
    val mappedMonad = monad.flatMapViaJoinAndMap(pure(n))((i: Int) => pure(i % 2 == 0))
    assertFs(mappedMonad, pure(n % 2 == 0))
  }

  test("Monad.composeViaJoinAndMap")(genIntList ** genString ** genRNG) { case intList ** s ** rng =>
    val tm = genMonad(rng)
    import tm.*
    val fThenG = monad.composeViaJoinAndMap(f, g)
    assertFs(fThenG(intList), pure(intList.sum.toString))
  }

  test("Id.map")(genIntList) { intList =>
    assertEquals(Id(intList).map(_.sum), Id(intList.sum))
  }

  test("Id.flatMap")(genIntList) { intList =>
    assertEquals(Id(intList).flatMap(list => Id(list.sum)), Id(intList.sum))
  }

  test("idMonad")(genInt ** genString) { case n ** s =>
    assertMonad[Id](idMonad, n, s)
  }

  test("readerMonad")(genInt ** genString) { case n ** s =>
    assertMonad[Reader[Unit, _]](readerMonad, n, s)
  }

  private def assertMonad[F[_]](tm: TestedMonad[F[_]], n: Int, s: String): Unit =
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

  private def assertAssociativeLaw[F[_]](tm: TestedMonad[F[_]], intList: List[Int]): Unit =
    import tm.*
    val fa = pure(intList)
    val fc1 = monad.flatMap(monad.flatMap(fa)(f))(g)
    val fc2 = monad.flatMap(fa)(a => monad.flatMap(f(a))(g))
    assertFs(fc1, fc2)

  private def assertAssociativeCompose[F[_]](tm: TestedMonad[F[_]], intList: List[Int]): Unit =
    import tm.*
    val fc1: List[Int] => F[Boolean] = monad.compose(monad.compose(f, g), h)
    val fc2: List[Int] => F[Boolean] = monad.compose(f, monad.compose(g, h))
    assertFs(fc1(intList), fc2(intList))

  private def assertIdentityLawForCompose[F[_]](tm: TestedMonad[F[_]], intList: List[Int]): Unit =
    import tm.*
    assertFs(monad.compose[List[Int], Int, Int](f, monad.unit)(intList), f(intList))
    assertFs(monad.compose[List[Int], List[Int], Int](monad.unit, f)(intList), f(intList))

  private def assertIdentityLawForFlatMap[F[_]](tm: TestedMonad[F[_]], intList: List[Int]): Unit =
    import tm.*
    assertFs(monad.flatMap(f(intList))(monad.unit), f(intList))
    assertFs(monad.flatMap(monad.unit(intList))(f), f(intList))

object MonadSuite extends Assertions:
  private trait TestedMonad[F[_]]:
    val monad: Monad[F]
    def pure[A]: A => F[A]
    def assertFs[A](actual: F[A], expected: F[A]): Unit =
      Assertions.assertEquals(actual, expected)
    def f: List[Int] => F[Int] = list => pure(list.sum)
    def g: Int => F[String] = i => pure(i.toString)
    def h: String => F[Boolean] = s => pure(s.startsWith("-"))

  private def genMonad(rng: RNG): TestedMonad[Gen[_]] =
    new TestedMonad[Gen]:
      val monad: Monad[Gen] = Monad.genMonad
      def pure[A]: A => Gen[A] = Gen.unit
      override def assertFs[A](actual: Gen[A], expected: Gen[A]): Unit = ???
        // ToDo: Uncomment after fpinscala.exercises.testing.GenSuite passing
        // Assertions.assertEquals(actual.next(rng)._1, expected.next(rng)._1)


  private val parMonad: TestedMonad[Par[_]] =
    new TestedMonad[Par]:
      val monad: Monad[Par] = Monad.parMonad
      def pure[A]: A => Par[A] = Par.unit
      override def assertFs[A](actual: Par[A], expected: Par[A]): Unit =
        Assertions.assertEquals(actual.run(service).get(), expected.run(service).get())

  private def parserMonad(s: String): TestedMonad[Parser[_]] =
    new TestedMonad[Parser]:
      val monad: Monad[Parser] = Monad.parserMonad(UnitTestParser)
      def pure[A]: A => Parser[A] = succeed
      override def assertFs[A](actual: Parser[A], expected: Parser[A]): Unit =
        Assertions.assertEquals(actual.run(s), expected.run(s))

  private val optionMonad: TestedMonad[Option[_]] =
    new TestedMonad[Option]:
      val monad: Monad[Option] = Monad.optionMonad
      def pure[A]: A => Option[A] = Some.apply

  private val lazyListMonad: TestedMonad[LazyList[_]] =
    new TestedMonad[LazyList]:
      val monad: Monad[LazyList] = Monad.lazyListMonad
      def pure[A]: A => LazyList[A] = a => LazyList(a)

  private val listMonad: TestedMonad[List[_]] =
    new TestedMonad[List]:
      val monad: Monad[List] = Monad.listMonad
      def pure[A]: A => List[A] = a => List(a)

  private val idMonad: TestedMonad[Id[_]] =
    new TestedMonad[Id]:
      val monad: Monad[Id] = Id.idMonad
      def pure[A]: A => Id[A] = a => Id(a)

  private val readerMonad: TestedMonad[Reader[Unit, _]] =
    new TestedMonad[Reader[Unit, _]]:
      val monad: Monad[Reader[Unit, _]] = Reader.readerMonad[Unit]
      def pure[A]: A => Reader[Unit, A] = monad.unit
      override def assertFs[A](actual: Reader[Unit, A], expected: Reader[Unit, A]): Unit =
        Assertions.assertEquals(actual.run(()), expected.run(()))

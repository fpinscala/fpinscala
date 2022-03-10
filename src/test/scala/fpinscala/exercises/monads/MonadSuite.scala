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
    val listMonad = tm.monad.sequence(intList.map(Gen.unit))
    assertEquals(tm.get(listMonad), intList)
  }

  test("Monad.traverse")(genIntList ** genRNG) { case intList ** rng =>
    val tm = genMonad(rng)
    val listMonad = tm.monad.traverse(intList)(Gen.unit)
    assertEquals(tm.get(listMonad), intList)
  }

  // Uncomment after fpinscala.exercises.testing.GenSuite passing
  /*
  test("Monad.replicateM")(genShortNumber ** genString ** genRNG) { case n ** s ** rng =>
    val tm = genMonad(rng)
    val listMonad = tm.monad.replicateM(n, Gen.choose(0, 1000))
    val intList: List[Int] = tm.get(listMonad)
    assertEquals(intList.length, n)
    assert(intList.forall(i => 0 <= i && i <= 1000))
  }
  */

  test("Monad.filterM")(genIntList ** genRNG) { case intList ** rng =>
    val tm = genMonad(rng)
    val filteredListM = tm.monad.filterM(intList)(i => Gen.unit(i % 2 == 0))
    val filteredList = tm.get(filteredListM)
    assertEquals(filteredList, intList.filter(_ % 2 == 0))
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
    val fThenG = tm.monad.compose(tm.f, tm.g)
    assertEquals(tm.get(fThenG(intList)), intList.sum.toString)
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
    val mappedMonad = tm.monad.flatMapViaCompose(tm.pure(n))((i: Int) => tm.pure(i % 2 == 0))
    val isNEven = tm.get(mappedMonad)
    assertEquals(isNEven, n % 2 == 0)
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
    val ffa = tm.monad.join(Gen.unit(Gen.unit(n)))
    val fa = Gen.unit(n)
    assertEquals(tm.get(ffa), tm.get(fa))
  }

  test("Monad.flatMapViaJoinAndMap")(genInt ** genRNG) { case n ** rng =>
    val tm = genMonad(rng)
    val mappedMonad = tm.monad.flatMapViaJoinAndMap(tm.pure(n))((i: Int) => tm.pure(i % 2 == 0))
    val isNEven = tm.get(mappedMonad)
    assertEquals(isNEven, n % 2 == 0)
  }

  test("Monad.composeViaJoinAndMap")(genIntList ** genString ** genRNG) { case intList ** s ** rng =>
    val tm = genMonad(rng)
    val fThenG = tm.monad.composeViaJoinAndMap(tm.f, tm.g)
    assertEquals(tm.get(fThenG(intList)), intList.sum.toString)
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
    assertEquals(tm.get[Int](tm.monad.unit(n)), n)

  private def assertFlatMap[F[_]](tm: TestedMonad[F[_]], n: Int): Unit =
    val m = tm.monad
    val appliedFlatMap = m.flatMap(m.unit(n))(i => m.unit(i % 2 == 0))
    val isNEven = tm.get[Boolean](appliedFlatMap)
    assertEquals(isNEven, n % 2 == 0)

  private def assertMap[F[_]](tm: TestedMonad[F[_]], n: Int): Unit =
    val m = tm.monad
    val appliedMap = m.map(m.unit(n))(_ % 2 == 0)
    val isNEven = tm.get[Boolean](appliedMap)
    assertEquals(isNEven, n % 2 == 0)

  private def assertMap2[F[_]](tm: TestedMonad[F[_]], n: Int, s: String): Unit =
    val m = tm.monad
    val appliedMap2 = m.map2(m.unit(n))(m.unit(s))((_, _))
    val tuple = tm.get[(Int, String)](appliedMap2)
    assertEquals(tuple, (n, s))

  private def assertAssociativeLaw[F[_]](tm: TestedMonad[F[_]], intList: List[Int]): Unit =
    assertAssociativeLaw[List[Int], Int, String, F[_]](tm.monad, tm.pure(intList), tm.f, tm.g, tm.get)

  private def assertAssociativeLaw[A, B, C, F[_]](
      m: Monad[F[_]],
      fa: F[A],
      f: A => F[B],
      g: B => F[C],
      get: F[C] => C
  ): Unit =
    val fc1: F[C] = m.flatMap(m.flatMap(fa)(f))(g)
    val fc2: F[C] = m.flatMap(fa)(a => m.flatMap(f(a))(g))
    assertEquals(get(fc1), get(fc2))

  private def assertAssociativeCompose[F[_]](tm: TestedMonad[F[_]], intList: List[Int]): Unit =
    assertAssociativeCompose[List[Int], Int, String, Boolean, F[_]](
      tm.monad,
      intList,
      tm.f,
      tm.g,
      tm.h,
      tm.get
    )

  private def assertAssociativeCompose[A, B, C, D, F[_]](
      m: Monad[F[_]],
      a: A,
      f: A => F[B],
      g: B => F[C],
      h: C => F[D],
      get: F[D] => D
  ): Unit =
    val fc1: A => F[D] = m.compose(m.compose(f, g), h)
    val fc2: A => F[D] = m.compose(f, m.compose(g, h))
    assertEquals(get(fc1(a)), get(fc2(a)))

  private def assertIdentityLawForCompose[F[_]](tm: TestedMonad[F[_]], intList: List[Int]): Unit =
    assertIdentityLawForCompose[List[Int], Int, F[_]](tm.monad, intList, tm.f, tm.get)

  private def assertIdentityLawForCompose[A, B, F[_]](m: Monad[F[_]], a: A, f: A => F[B], get: F[B] => B): Unit =
    assertEquals(get(m.compose[A, B, B](f, m.unit)(a)), get(f(a)))
    assertEquals(get(m.compose[A, A, B](m.unit, f)(a)), get(f(a)))

  private def assertIdentityLawForFlatMap[F[_]](tm: TestedMonad[F[_]], intList: List[Int]): Unit =
    assertIdentityLawForFlatMap[List[Int], Int, F[_]](tm.monad, intList, tm.f, tm.get)

  private def assertIdentityLawForFlatMap[A, B, F[_]](m: Monad[F[_]], a: A, f: A => F[B], get: F[B] => B): Unit =
    assertEquals(get(m.flatMap(f(a))(m.unit)), get(f(a)))
    assertEquals(get(m.flatMap(m.unit(a))(f)), get(f(a)))

object MonadSuite:
  private trait TestedMonad[F[_]]:
    val monad: Monad[F]
    def get[A]: F[A] => A
    def pure[A]: A => F[A]
    def f: List[Int] => F[Int] = list => pure(list.sum)
    def g: Int => F[String] = i => pure(i.toString)
    def h: String => F[Boolean] = s => pure(s.startsWith("-"))

  private def genMonad(rng: RNG): TestedMonad[Gen[_]] =
    new TestedMonad[Gen] {
      val monad: Monad[Gen] = Monad.genMonad
      // Uncomment after fpinscala.exercises.testing.GenSuite passing
      def get[A]: Gen[A] => A = ??? // _.next(rng)._1
      def pure[A]: A => Gen[A] = Gen.unit
    }

  private val parMonad: TestedMonad[Par[_]] =
    new TestedMonad[Par] {
      val monad: Monad[Par] = Monad.parMonad
      def get[A]: Par[A] => A = _.run(service).get()
      def pure[A]: A => Par[A] = Par.unit
    }

  private def parserMonad(s: String): TestedMonad[Parser[_]] =
    new TestedMonad[Parser] {
      val monad: Monad[Parser] = Monad.parserMonad(UnitTestParser)
      def get[A]: Parser[A] => A = _.run(s).toOption.get
      def pure[A]: A => Parser[A] = succeed
    }

  private val optionMonad: TestedMonad[Option[_]] =
    new TestedMonad[Option] {
      val monad: Monad[Option] = Monad.optionMonad
      def get[A]: Option[A] => A = _.get
      def pure[A]: A => Option[A] = Some.apply
    }

  private val lazyListMonad: TestedMonad[LazyList[_]] =
    new TestedMonad[LazyList] {
      val monad: Monad[LazyList] = Monad.lazyListMonad
      def get[A]: LazyList[A] => A = _.head
      def pure[A]: A => LazyList[A] = a => LazyList(a)
    }

  private val listMonad: TestedMonad[List[_]] =
    new TestedMonad[List] {
      val monad: Monad[List] = Monad.listMonad
      def get[A]: List[A] => A = _.head
      def pure[A]: A => List[A] = a => List(a)
    }

  private val idMonad: TestedMonad[Id[_]] =
    new TestedMonad[Id] {
      val monad: Monad[Id] = Id.idMonad
      def get[A]: Id[A] => A = _.value
      def pure[A]: A => Id[A] = a => Id(a)
    }

  private val readerMonad: TestedMonad[Reader[Unit, _]] =
    new TestedMonad[Reader[Unit, _]] {
      val monad: Monad[Reader[Unit, _]] = Reader.readerMonad[Unit]
      def get[A]: Reader[Unit, A] => A = _.run(())
      def pure[A]: A => Reader[Unit, A] = monad.unit
    }

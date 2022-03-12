package fpinscala.exercises.common

import fpinscala.answers.testing.exhaustive.Gen
import fpinscala.exercises.state.RNG

import java.util.concurrent.{ExecutorService, Executors}

object Common:
  lazy val service: ExecutorService = Executors.newFixedThreadPool(4)
  lazy val theFirst21FibonacciNumbers =
    IndexedSeq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765)
  lazy val genLengthOfFibonacciSeq: Gen[Int] = Gen.choose(0, theFirst21FibonacciNumbers.length)
  lazy val genShortNumber: Gen[Int] = Gen.choose(0, 20)
  lazy val genChar: Gen[Char] = Gen.choose(97, 123).map(_.toChar)
  lazy val genString: Gen[String] = genList(genChar).map(_.mkString)
  lazy val genRNG: Gen[RNG] = Gen.int.map(i => RNG.Simple(i.toLong))
  lazy val genIntList: Gen[List[Int]] = genList(Gen.int)
  lazy val genDoubleList: Gen[List[Double]] = genList(Gen.double)
  lazy val genStringList: Gen[List[String]] = genList(genString)
  lazy val genBooleanList: Gen[List[Boolean]] = genList(Gen.boolean)
  lazy val genIntOption: Gen[Option[Int]] = Gen.int.map(i => if i % 2 == 0 then Some(i / 2) else None)
  lazy val genIntIndexedSeq: Gen[IndexedSeq[Int]] = genList(Gen.int).map(_.toIndexedSeq)
  lazy val genDoubleIndexedSeq: Gen[IndexedSeq[Double]] = genList(Gen.double).map(_.toIndexedSeq)
  lazy val genStringIndexedSeq: Gen[IndexedSeq[String]] = genList(genString).map(_.toIndexedSeq)
  lazy val genBooleanIndexedSeq: Gen[IndexedSeq[Boolean]] = genList(Gen.boolean).map(_.toIndexedSeq)
  lazy val genIntLazyList: Gen[LazyList[Int]] = genLazyList(Gen.int)
  lazy val genDoubleLazyList: Gen[LazyList[Double]] = genLazyList(Gen.double)
  lazy val genStringLazyList: Gen[LazyList[String]] = genLazyList(genString)
  lazy val genBooleanLazyList: Gen[LazyList[Boolean]] = genLazyList(Gen.boolean)

  def genList[A](g: Gen[A]): Gen[List[A]] =
    for
      n <- genShortNumber
      list <- Gen.listOfN(n, g)
    yield list

  def genNonEmptyList[A](g: Gen[A]): Gen[List[A]] =
    for
      n <- Gen.choose(1, 20)
      list <- Gen.listOfN(n, g)
    yield list

  def genLazyList[A](g: Gen[A]): Gen[LazyList[A]] =
    def loop(): Gen[LazyList[A]] =
      Gen.boolean.flatMap { b =>
        if b then Gen.unit(LazyList.empty)
        else
          for
            head <- g
            tail <- loop()
          yield LazyList.cons(head, tail)
      }
    loop()

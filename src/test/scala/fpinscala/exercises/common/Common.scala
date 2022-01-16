package fpinscala.exercises.common

import fpinscala.answers.testing.exhaustive.Gen

object Common:
  val theFirst21FibonacciNumbers =
    IndexedSeq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765)
  val genLengthOfFibonacciSeq: Gen[Int] = Gen.choose(0, theFirst21FibonacciNumbers.length)
  val genShortNumber: Gen[Int] = Gen.choose(0, 20)
  val genChar: Gen[Char] = Gen.choose(97, 123).map(_.toChar)
  val genString: Gen[String] = genList(genChar).map(_.mkString)
  val genIntList: Gen[List[Int]] = genList(Gen.int)
  val genDoubleList: Gen[List[Double]] = genList(Gen.double)
  val genStringList: Gen[List[String]] = genList(genString)

  def genList[A](g: Gen[A]): Gen[List[A]] =
    for
      n <- genShortNumber
      list <- Gen.listOfN(n, g)
    yield list

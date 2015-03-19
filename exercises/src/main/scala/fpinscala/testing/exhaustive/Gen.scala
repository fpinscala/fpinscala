package fpinscala.testing.exhaustive

import fpinscala.parallelism.Nonblocking.Par
import fpinscala.parallelism.Nonblocking.Par.ParOps

sealed trait Gen[+A] {
  def stream(rng: RNG): Stream[A]

  def isInfinite: Boolean

  def toSample: SampleGen[A]
}

object Gen {
  def unit[A](a: => A): SampleGen[A] = SampleGen(RNG.unit(a))

  def choose(start: Int, stopExclusive: Int): SampleGen[Int] =
    SampleGen(RNG.nonNegativeLessThan(stopExclusive - start).map(_ + start))

  def values[A](a: A*): DomainGen[A] = DomainGen(a.toVector)

  def int: SampleGen[Int] = SampleGen(RNG.int)
  def nonNegativeInt: SampleGen[Int] = SampleGen(RNG.nonNegativeInt)
  def double: SampleGen[Double] = SampleGen(RNG.double)

  def boolean: DomainGen[Boolean] = values(true, false)

  def listOfN[A](n: Int, g: SampleGen[A]): SampleGen[List[A]] = SampleGen(State { initialRng =>
    (0 until n).foldRight(List.empty[A] -> initialRng) {
      case (_, (tail, rng)) =>
        g.sample.map(_ :: tail).run(rng)
    }
  })

  def weighted[A](g1: (SampleGen[A], Double), g2: (SampleGen[A], Double)): SampleGen[A] = {
    val sum = g1._2 + g2._2
    double.flatMap { r =>
      if (r * sum < g1._2) g1._1
      else g2._1
    }
  }

  def pint: Gen[Par[Int]] =
    for {
      list <- choose(-10, 10).listOfN(Gen.choose(100, 200))
    } yield for {
      sorted <- Par.forkValue(list.sorted)
      parSorted = sorted.map(Par.unit)
      reSorted <- Par.fork(Par.sequence(parSorted))
    } yield reSorted.headOption.getOrElse(0)

  implicit class GenWeightOps[A](val gen: (SampleGen[A], Double)) {
    def union[B >: A](other: (SampleGen[B], Double)): SampleGen[B] = Gen.weighted(gen, other)
  }
}

case class DomainGen[+A](values: Vector[A]) extends Gen[A] {
  override def isInfinite: Boolean = false
  override def stream(rng: RNG): Stream[A] = values.toStream

  override def toSample: SampleGen[A] = SampleGen {
    RNG.nonNegativeLessThan(values.length).map(values)
  }
}

object DomainGen {
  def apply[A](values: TraversableOnce[A]): DomainGen[A] =
    DomainGen(values.toVector)
}

case class SampleGen[+A](sample: Rand[A]) extends Gen[A] {
  import Gen._

  def map[B](f: A => B): SampleGen[B] = flatMap { a => unit(f(a)) }

  def flatMap[B](f: A => SampleGen[B]): SampleGen[B] =
    SampleGen(sample.flatMap { a => f(a).sample })

  def listOfN(size: Int): SampleGen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: SampleGen[Int]): SampleGen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))

  def union[B >: A](that: SampleGen[B]): SampleGen[B] =
    Gen.boolean.toSample.flatMap(if (_) this else that)

  override def isInfinite = true

  override def stream(rng: RNG): Stream[A] =
    RNG.stream(sample)(rng)

  override def toSample: SampleGen[A] = this
}

case class SizedGen[+A](forSize: Int => SampleGen[A])

object SizedGen {
  def sampleListOf[A](g: Gen[A]): SizedGen[List[A]] = SizedGen(g.toSample.listOfN)
  def sampleListOf1[A](g: Gen[A]): SizedGen[List[A]] = SizedGen { size => g.toSample.listOfN(size max 1) }
}


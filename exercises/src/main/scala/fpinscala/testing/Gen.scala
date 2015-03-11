package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))

  def int: Gen[Int] = Gen(State(RNG.int))
  def nonNegativeInt: Gen[Int] = Gen(State(RNG.nonNegativeInt))
  def double: Gen[Double] = Gen(State(RNG.double))

  def boolean: Gen[Boolean] = int.map(_ % 2 == 0)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State { initialRng =>
    (0 until n).foldRight(List.empty[A] -> initialRng) {
      case (_, (tail, rng)) =>
        g.sample.map(_ :: tail).run(rng)
    }
  })

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val sum = g1._2 + g2._2
    double.flatMap { r =>
      if (r * sum < g1._2) g1._1
      else g2._1
    }
  }

  implicit class GenWeightOps[A](val gen: (Gen[A], Double)) {
    def union[B >: A](other: (Gen[B], Double)): Gen[B] = Gen.weighted(gen, other)
  }
}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = flatMap { a => unit(f(a)) }

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap { a => f(a).sample })

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))

  def union[B >: A](that: Gen[B]): Gen[B] =
    Gen.boolean.flatMap(if (_) this else that)
}

trait SGen[+A] {

}


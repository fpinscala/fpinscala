package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

import scala.util.control.NonFatal

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified = false
}

case class Falsified(label: Option[String], failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified = true
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(that: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Passed => that.run(n, rng)
      case falsified => falsified
    }
  }

  def ||(that: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Passed => Passed
      case _ => that.run(n, rng)
    }
  }
}

class PropBuilder(label: Option[String]) {

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(label, a.toString, i)
        } catch {
          case NonFatal(e) => Falsified(label, buildMsg(a, e), i)
        }
    }.find(_.isFalsified).getOrElse(Passed)
  }
}

object Prop extends PropBuilder(None) {

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def apply(label: String): PropBuilder = new PropBuilder(Some(label))

  def randomStream[A](g: Gen[A])(rng1: RNG): Stream[A] =
    Stream.unfold(rng1) { rng2 => Some(g.sample.run(rng2)) }

  def buildMsg[A](s: A, e: Throwable): String =
    s"""
       |test case: $s
       |generated an exception: ${e.getMessage}
       |stack trace:
       |${e.getStackTrace.mkString("\n")}
     """.stripMargin
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


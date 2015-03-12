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

case object Proved extends Result {
  override def isFalsified = false
}

case class Falsified(label: Option[String], failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(that: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => that.run(max, n, rng)
      case f @ Falsified(_, _, _) => f
    }
  }

  def ||(that: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => Passed
      case Falsified(_, _, _) => that.run(max, n, rng)
    }
  }
}

class PropBuilder(label: Option[String]) {

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(label, a.toString, i)
        } catch {
          case NonFatal(e) => Falsified(label, buildMsg(a, e), i)
        }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props = Stream.from(0).take((n min max) + 1).map { i => forAll(g(i))(f) }

    val prop = props.map { p =>
      Prop { (max, _, rng) => p.run(max, casesPerSize, rng) }
    }.toList.reduce(_ && _)
    prop.run(max, n, rng)
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified(label, "()", 0)
  }
}

object Prop extends PropBuilder(None) {

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

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

  def unsized: SGen[A] = SGen { _ => this }
}

case class SGen[+A](forSize: Int => Gen[A])

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(g.listOfN)
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen { size => g.listOfN(size max 1) }
}


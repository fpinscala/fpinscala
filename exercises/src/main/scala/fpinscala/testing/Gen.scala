package fpinscala.testing

import java.util.concurrent.Executors

import fpinscala.laziness.Stream
import fpinscala.monoids.Monoid
import fpinscala.monoids.Monoid.{Part, Stub}
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.{Par, ParOps}
import fpinscala.state._
import fpinscala.testing.Prop._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

object Prop {
  type MaxSize = Int
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
    randomStream(gen)(rng).zipWith(Stream.from(0))((_, _)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(errorMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }
  }

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(Gen.es ** g) { case s ** a => f(a)(s).get }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Proved =>
        println(s"yo!\n your prop is proved")
      case Passed =>
        println(s"yo!\n your prop is correct, total - $testCases cases succeed. that is awesome man, i'm very proud of you")
      case Falsified(failure, successes) =>
        println(s"oh no :(\n nevertheless $successes cases succeed, this one failed - $failure")
    }
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def checkPar(f: => Par[Boolean]): Prop =
    forAll(Gen.es)(s => f(s).get)

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def errorMsg[A](s: A, e: Exception): String =
    s"""test case: $s
       |error: $e
       |""".stripMargin
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Proved extends Result {
  def isFalsified: Boolean = false
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def unit(r: Result): Prop = Prop { (_, _, _) => r }

  def flatMap(f: Result => Prop): Prop = Prop { (max, n, rng) =>
    f(this.run(max, n, rng)).run(max, n, rng)
  }

  def &&(p: Prop): Prop = flatMap({
    case Passed | Proved => p
    case f => unit(f)
  })

  def ||(p: Prop): Prop = flatMap({
    case Passed => unit(Passed)
    case Proved => unit(Proved)
    case Falsified(failure, _) => p.tag(failure)
  })

  def tag(prefix: String): Prop = flatMap({
    case Falsified(failure, successes) =>
      unit(Falsified(
        s"""$prefix:
           |$failure""".stripMargin, successes))
    case Passed => unit(Passed)
  })
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = new Gen[B](sample.map(f(_)))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = this.flatMap(a => g.map(b => f(a, b)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = new Gen[B](sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)

  def listOf: SGen[List[A]] = SGen(n => Gen.listOfN(n, this))

  def listOf1: SGen[List[A]] = SGen(n => if (n == 0) Gen.listOfN(n + 1, this) else Gen.listOfN(n, this))

  def **[B](g: Gen[B]): Gen[(A, B)] = this.map2(g)((_, _))
}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.map(RNG.int)(_ > 0)))

  def int: Gen[Int] = Gen(State(RNG.int))

  def double: Gen[Double] = Gen(State(RNG.double))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(Seq.fill(n)(g.sample).toList))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.map(RNG.nonNegativeLessThan(stopExclusive - start + 1))(_ + start)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    double.flatMap(d => if (d <= (g1._2 / (g1._2 + g2._2))) g1._1 else g2._1)

  def es = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  import fpinscala.errorhandling._

  def option[A](g: Gen[A]): Gen[Option[A]] = weighted(
    (g.map(x => Some(x)), .8),
    (unit(None), .2)
  )

  def stringN(gn: Gen[Int]): Gen[String] =
    gn.flatMap(n =>
      listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString))

  def trueOption[A](g: Gen[A]): Gen[scala.Option[A]] = weighted(
    (g.map(x => scala.Some(x)), .8),
    (unit(scala.None), .2)
  )

  def wc(gl: Gen[String], gw: Gen[Int], gr: Gen[String]): Gen[Monoid.WC] = weighted(
    ((gl ** gw ** gr).map { case l ** w ** r => Part(l, w, r) }, .8),
    (gl.map(Stub), .2)
  )

  def endoIntFunction(g: Gen[Int]): Gen[Int => Int] = weighted(
    (weighted((g.map { g => _ + g }, .5), (g.map { g => _ - g }, .5)), .5),
    (weighted((g.map { g => _ * g }, .5), (g.map { g => _ / g }, .5)), .5)
  )
}

case class SGen[A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n =>
    forSize(n).flatMap(a => f(a).forSize(n))
  )

  def listOfN(size: Gen[Int]): SGen[List[A]] = SGen(n => size.flatMap(Gen.listOfN(_, forSize(n))))
}

object App extends App {
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  // sum, sort

  val maxProp = forAll(Gen.choose(-10, 10).listOf1) { l =>
    val max = l.max
    l.forall(_ <= max)
  }

  val sortProp1 = forAll(Gen.choose(-10, 10).listOf) { l =>
    val sorted = l.sorted
    val res = for {
      h <- sorted.headOption
      l <- sorted.lastOption
    } yield h <= l

    res.getOrElse(true)
  }

  val sortProp2 = forAll(Gen.choose(-10, 10).listOf1) { l =>
    l.sorted == l.reverse.sorted
  }

  run(maxProp)
  run(sortProp1 && sortProp2)

  // parallel

  val es = Executors.newCachedThreadPool
  val p1 = forAll(Gen.unit(Par.unit(1))) { i =>
    Par.map(i)(_ + 1)(es).get == Par.unit(2)(es).get
  }

  val p2 = check {
    Par.map(Par.unit(1))(_ + 1)(es).get ==
      Par.unit(2)(es).get
  }

  val p3 = checkPar {
    Par.map(Par.unit(1))(_ + 1) eq2 Par.unit(2)
  }

  val simpleParIntGen = Gen.choose(-10, 10) map Par.unit

  val complexParIngGen = (
    Gen.choose(-10, 10) **
      Gen.choose(-10, 10) **
      Gen.choose(-10, 10) **
      Gen.choose(-10, 10)
    ) map {
    case a ** b ** c ** d =>
      val ab = Par.map2(Par.unit(a), Par.unit(b))(_ * _)
      val cd = Par.map2(Par.unit(c), Par.unit(d))(_ * _)
      Par.map2(ab, cd)(_ + _)
  }

  val p4 = forAllPar(complexParIngGen)(x => Par.map(x)(identity) eq2 x) tag "map"

  val forkP = forAllPar(complexParIngGen)(x => Par.fork(x) eq2 x) tag "fork"

  run(p1)
  run(p2)
  run(p3)
  run(p4)
  run(forkP)

  // streams

  val pTake = forAll(Gen.int.listOfN(Gen.choose(0, 123)) ** Gen.choose(0, 30)) {
    case list ** n =>
      if (n < list.size) list.take(n).size == n else true
  }

  val pFilter = forAll(Gen.int.listOfN(Gen.choose(0, 123)) ** Gen.choose(0, 30)) {
    case list ** n =>
      list.filter(_ < n).forall(_ < n)
  }

  import fpinscala.errorhandling._

  val seqFilter = forAll(Gen.option(Gen.int).listOfN(Gen.choose(0, 15)))(list => {
    val res: Option[List[Int]] = Option.sequence2(list)
    res == (if (list.contains(None)) None else Some(list.map(_.getOrElse(0))))
  })

  run(pTake)
  run(pFilter)
  run(seqFilter)

}
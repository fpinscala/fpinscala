package fpinscala.testing.exhaustive

import scala.util.control.NonFatal

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified = false
}

case object Proved extends Result {
  override def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(that: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => that.run(max, n, rng)
      case f @ Falsified(_, _) => f
    }
  }

  def ||(that: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => Passed
      case Falsified(_, _) => that.run(max, n, rng)
    }
  }
}

object Prop {

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    as.stream(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case NonFatal(e) => Falsified(buildMsg(a, e), i)
        }
    }.find(_.isFalsified).getOrElse(if (as.isInfinite) Passed else Proved)
  }

  def forAll[A](g: SizedGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => SampleGen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props = Stream.from(0).take((n min max) + 1).map { i => forAll(g(i))(f) }

    val prop = props.map { p =>
      Prop { (max, _, rng) => p.run(max, casesPerSize, rng) }
    }.toList.reduce(_ && _)
    prop.run(max, n, rng)
  }

  def check(p: => Boolean): Prop = forAll(Gen.values(())) { _ => p }

  def buildMsg[A](s: A, e: Throwable): String =
    s"""
       |test case: $s
        |generated an exception: ${e.getMessage}
        |stack trace:
        |${e.getStackTrace.mkString("\n")}
     """.stripMargin

}
package fpinscala.exercises.common

import fpinscala.answers.state.RNG
import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.answers.testing.exhaustive.Prop.Result.*
import fpinscala.answers.testing.exhaustive.Prop.Status.*
import munit.*
import munit.internal.FutureCompat.*

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait PropSuite extends FunSuite:
  def test[A](name: String)(a: Gen[A])(f: A => Unit)(implicit loc: Location): Unit =
    val g: A => Boolean =
      a =>
        f(a)
        true
    val prop = forAll[A](a)(g)
    test(new TestOptions(name, Set.empty, loc))(prop.check())

  override def munitTestTransforms: List[TestTransform] =
    super.munitTestTransforms :+ scalaCheckPropTransform

  private val scalaCheckPropTransform: TestTransform =
    new TestTransform(
      "FPInScala Prop",
      t =>
        t.withBodyMap(
          _.transformCompat {
            case Success(result: Result) => resultToTry(result, t)
            case r                       => r
          }(munitExecutionContext)
        )
    )

  private def resultToTry(result: Result, test: Test): Try[Unit] =
    result match
      case Passed(status, n) =>
        println(s"${test.name}: + OK, property ${status.toString.toLowerCase}, ran $n tests.")
        Success(())
      case Falsified(msg) =>
        Try(fail(msg.string)(test.location))

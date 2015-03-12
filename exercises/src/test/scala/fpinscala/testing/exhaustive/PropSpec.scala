package fpinscala.testing.exhaustive

import org.specs2.mutable._
import Prop._
import Gen._

object PropSpec {
  implicit class PropOps(val p: Prop) extends AnyVal {
    def quickRun = p.run(100, 100, RNG.Simple(0))
  }
}
class PropSpec extends Specification {

  import PropSpec._

  "forAll" should {
    "execute property checks with success" in {
      val props = forAll(choose(10, 20)) { i => i >= 10 && i < 20 }
      val result = props.quickRun

      result.isFalsified must beFalse
    }

    "execute property checks with failure" in {
      val props = forAll(choose(15, 25)) { i => i >= 10 && i < 20 }
      val result = props.quickRun

      result match {
        case Passed | Proved => failure("The check shouldn't have been successful.")
        case Falsified(failedCase, successes) =>
          failedCase.toInt must beBetween(20, 25).excludingEnd
      }
      success
    }
  }

  "&&" should {
    "combine two property checks" in {
      val props =
        forAll(choose(10, 20)) { i => i >= 10 && i < 20 } &&
        forAll(choose(100, 200)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result.isFalsified must beFalse
    }

    "combine report failure for the first side that failed" in {
      val props =
        forAll(Gen.choose(15, 25)) { i => i >= 10 && i < 20 } &&
        forAll(Gen.choose(100, 200)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result match {
        case Passed | Proved => failure("The check shouldn't have been successful.")
        case Falsified(failedCase, successes) =>
          failedCase.toInt must beBetween(20, 25).excludingEnd
      }
      success
    }

    "combine report failure for the second if it failed" in {
      val props =
        forAll(choose(10, 20)) { i => i >= 10 && i < 20 } &&
        forAll(choose(150, 250)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result match {
        case Passed | Proved => failure("The check shouldn't have been successful.")
        case Falsified(failedCase, successes) =>
          failedCase.toInt must beBetween(200, 250).excludingEnd
      }
      success
    }
  }

  "||" should {
    "pass even if the first test fails" in {
      val props =
        forAll(choose(20, 30)) { i => i >= 10 && i < 20 } ||
        forAll(choose(100, 200)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result.isFalsified must beFalse
    }
    "pass even if the second test fails" in {
      val props =
        forAll(choose(10, 20)) { i => i >= 10 && i < 20 } ||
        forAll(choose(200, 300)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result.isFalsified must beFalse
    }
    "pass even if both tests pass" in {
      val props =
        forAll(choose(10, 20)) { i => i >= 10 && i < 20 } ||
        forAll(choose(100, 200)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result.isFalsified must beFalse
    }
    "fail if both tests fail" in {
      val props =
        forAll(Gen.choose(20, 30)) { i => i >= 10 && i < 20 } ||
        forAll(Gen.choose(150, 250)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result match {
        case Passed | Proved => failure("The check shouldn't have been successful.")
        case Falsified(failedCase, successes) =>
          failedCase.toInt must beBetween(200, 250).excludingEnd
      }
      success
    }
  }

  "forAll#SGen" should {
    "validate max properties" in {
      val maxProp = forAll(SizedGen.sampleListOf1(Gen.choose(-10, 10))) { ns =>
        val max = ns.max
        !ns.exists(_ > max)
      }

      maxProp.quickRun match {
        case Passed | Proved => success
        case Falsified(message, n) =>
          failure(s"Falsified after $n passed tests: $message")
      }
    }
    "validate sorted properties" in {
      val maxProp = forAll(SizedGen.sampleListOf1(Gen.choose(-10, 10))) { ns =>
        val sorted = ns.sorted
        (sorted zip sorted.tail) forall { case (a, b) => a <= b }
      }

      maxProp.quickRun match {
        case Passed | Proved => success
        case Falsified(message, n) =>
          failure(s"Falsified after $n passed tests: $message")
      }
    }
  }

  "forAll#DomainGen" should {
    "Exhaustively check a domain" in {
      forAll(DomainGen(1 to 100)) { i => i < 1 || i > 100 } quickRun match {
        case Proved => success
        case _ => failure
      }
    }
  }

}

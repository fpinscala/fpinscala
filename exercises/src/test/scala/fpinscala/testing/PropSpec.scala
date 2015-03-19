package fpinscala.testing

import fpinscala.state.RNG
import org.specs2.mutable._

object PropSpec {
  implicit class PropOps(val p: Prop) extends AnyVal {
    def quickRun = p.run(100, 100, RNG.Simple(0))
  }
}
class PropSpec extends Specification {

  import PropSpec._

  def validProperty1 = Prop("validProperty1")
    .forAll(Gen.choose(10, 20)) { i => i >= 10 && i < 20 }

  def validProperty2 = Prop("validProperty2")
    .forAll(Gen.choose(100, 200)) { i => i >= 100 && i < 200 }

  def invalidProperty1 = Prop("invalidProperty1")
    .forAll(Gen.choose(20, 30)) { i => i >= 10 && i < 20 }

  def invalidProperty2 = Prop("invalidProperty2")
    .forAll(Gen.choose(150, 250)) { i => i >= 100 && i < 200 }

  "forAll" should {
    "execute property checks with success" in {
      val result = validProperty1.quickRun

      result.isFalsified must beFalse
    }

    "execute property checks with failure" in {
      val result = invalidProperty1.quickRun

      result match {
        case Passed | Proved => failure("The check shouldn't have been successful.")
        case Falsified(label, failedCase, successes) =>
          failedCase.toInt must beBetween(20, 30).excludingEnd
          label must be some "invalidProperty1"
      }
      success
    }
  }

  "&&" should {
    "combine two property checks" in {
      val props = validProperty1 && validProperty2
      val result = props.quickRun

      result.isFalsified must beFalse
    }

    "combine report failure for the first side that failed" in {
      val props = invalidProperty1 && validProperty1

      val result = props.quickRun

      result match {
        case Passed | Proved => failure("The check shouldn't have been successful.")
        case Falsified(label, failedCase, successes) =>
          failedCase.toInt must beBetween(20, 30).excludingEnd
          label must be some "invalidProperty1"
      }
      success
    }

    "combine report failure for the second if it failed" in {
      val props = validProperty1 && invalidProperty1
      val result = props.quickRun

      result match {
        case Passed | Proved => failure("The check shouldn't have been successful.")
        case Falsified(label, failedCase, successes) =>
          failedCase.toInt must beBetween(20, 30).excludingEnd
          label must be some "invalidProperty1"
      }
      success
    }
  }

  "||" should {
    "pass even if the first test fails" in {
      val props = invalidProperty1 || validProperty1
      val result = props.quickRun

      result.isFalsified must beFalse
    }
    "pass even if the second test fails" in {
      val props = validProperty1 || invalidProperty1
      val result = props.quickRun

      result.isFalsified must beFalse
    }
    "pass if both tests pass" in {
      val props = validProperty1 || validProperty2
      val result = props.quickRun

      result.isFalsified must beFalse
    }
    "fail if both tests fail" in {
      val props = invalidProperty1 || invalidProperty2

      val result = props.quickRun

      result match {
        case Passed | Proved => failure("The check shouldn't have been successful.")
        case Falsified(label, failedCase, successes) =>
          failedCase.toInt must beBetween(200, 250).excludingEnd
          label must be some "invalidProperty2"
      }
      success
    }
  }

  "forAll#SGen" should {
    "validate max properties" in {
      val maxProp = Prop.forAll(SGen.listOf1(Gen.choose(-10, 10))) { ns =>
        val max = ns.max
        !ns.exists(_ > max)
      }

      maxProp.quickRun match {
        case Passed | Proved => success
        case Falsified(label, message, n) =>
          failure(s"Falsified after $n passed tests: $message")
      }
    }
    "validate sorted properties" in {
      val maxProp = Prop.forAll(SGen.listOf1(Gen.choose(-10, 10))) { ns =>
        val sorted = ns.sorted
        (sorted zip sorted.tail) forall { case (a, b) => a <= b }
      }

      maxProp.quickRun match {
        case Passed | Proved => success
        case Falsified(label, message, n) =>
          failure(s"Falsified after $n passed tests: $message")
      }
    }
  }
}


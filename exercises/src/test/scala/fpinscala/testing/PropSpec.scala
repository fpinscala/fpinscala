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

  "forAll" should {
    "execute property checks with success" in {
      val props = Prop.forAll(Gen.choose(10, 20)) { i => i >= 10 && i < 20 }
      val result = props.quickRun

      result.isFalsified must beFalse
    }

    "execute property checks with failure" in {
      val props = Prop.forAll(Gen.choose(15, 25)) { i => i >= 10 && i < 20 }
      val result = props.quickRun

      result match {
        case Passed | Proved => failure("The check shouldn't have been successful.")
        case Falsified(label, failedCase, successes) =>
          failedCase.toInt must beBetween(20, 25).excludingEnd
          label must be none
      }
      success
    }
  }

  "&&" should {
    "combine two property checks" in {
      val props =
        Prop.forAll(Gen.choose(10, 20)) { i => i >= 10 && i < 20 } &&
        Prop.forAll(Gen.choose(100, 200)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result.isFalsified must beFalse
    }

    "combine report failure for the first side that failed" in {
      val props =
        Prop("first").forAll(Gen.choose(15, 25)) { i => i >= 10 && i < 20 } &&
        Prop("second").forAll(Gen.choose(100, 200)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result match {
        case Passed | Proved => failure("The check shouldn't have been successful.")
        case Falsified(label, failedCase, successes) =>
          failedCase.toInt must beBetween(20, 25).excludingEnd
          label must be some "first"
      }
      success
    }

    "combine report failure for the second if it failed" in {
      val props =
        Prop("first").forAll(Gen.choose(10, 20)) { i => i >= 10 && i < 20 } &&
        Prop("second").forAll(Gen.choose(150, 250)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result match {
        case Passed | Proved => failure("The check shouldn't have been successful.")
        case Falsified(label, failedCase, successes) =>
          failedCase.toInt must beBetween(200, 250).excludingEnd
          label must be some "second"
      }
      success
    }
  }

  "||" should {
    "pass even if the first test fails" in {
      val props =
        Prop.forAll(Gen.choose(20, 30)) { i => i >= 10 && i < 20 } ||
        Prop.forAll(Gen.choose(100, 200)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result.isFalsified must beFalse
    }
    "pass even if the second test fails" in {
      val props =
        Prop.forAll(Gen.choose(10, 20)) { i => i >= 10 && i < 20 } ||
        Prop.forAll(Gen.choose(200, 300)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result.isFalsified must beFalse
    }
    "pass even if both tests pass" in {
      val props =
        Prop.forAll(Gen.choose(10, 20)) { i => i >= 10 && i < 20 } ||
        Prop.forAll(Gen.choose(100, 200)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result.isFalsified must beFalse
    }
    "fail if both tests fail" in {
      val props =
        Prop("first").forAll(Gen.choose(20, 30)) { i => i >= 10 && i < 20 } ||
        Prop("second").forAll(Gen.choose(150, 250)) { i => i >= 100 && i < 200 }

      val result = props.quickRun

      result match {
        case Passed | Proved => failure("The check shouldn't have been successful.")
        case Falsified(label, failedCase, successes) =>
          failedCase.toInt must beBetween(200, 250).excludingEnd
          label must be some "second"
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

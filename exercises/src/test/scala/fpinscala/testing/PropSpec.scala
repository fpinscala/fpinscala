package fpinscala.testing

import fpinscala.state.RNG
import org.specs2.mutable._

class PropSpec extends Specification {

  "forAll" should {
    "execute property checks with success" in {
      val props = Prop.forAll(Gen.choose(10, 20)) { i => i >= 10 && i < 20 }
      val result = props.run(100, RNG.Simple(0))

      result.isFalsified must beFalse
    }

    "execute property checks with failure" in {
      val props = Prop.forAll(Gen.choose(15, 25)) { i => i >= 10 && i < 20 }
      val result = props.run(100, RNG.Simple(0))

      result match {
        case Passed => failure("The check shouldn't have been successful.")
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

      val result = props.run(100, RNG.Simple(0))

      result.isFalsified must beFalse
    }

    "combine report failure for the first side that failed" in {
      val props =
        Prop("first").forAll(Gen.choose(15, 25)) { i => i >= 10 && i < 20 } &&
          Prop("second").forAll(Gen.choose(100, 200)) { i => i >= 100 && i < 200 }

      val result = props.run(100, RNG.Simple(0))

      result match {
        case Passed => failure("The check shouldn't have been successful.")
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

      val result = props.run(100, RNG.Simple(0))

      result match {
        case Passed => failure("The check shouldn't have been successful.")
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

      val result = props.run(100, RNG.Simple(0))

      result.isFalsified must beFalse
    }
    "pass even if the second test fails" in {
      val props =
        Prop.forAll(Gen.choose(10, 20)) { i => i >= 10 && i < 20 } ||
          Prop.forAll(Gen.choose(200, 300)) { i => i >= 100 && i < 200 }

      val result = props.run(100, RNG.Simple(0))

      result.isFalsified must beFalse
    }
    "pass even if both tests pass" in {
      val props =
        Prop.forAll(Gen.choose(10, 20)) { i => i >= 10 && i < 20 } ||
          Prop.forAll(Gen.choose(100, 200)) { i => i >= 100 && i < 200 }

      val result = props.run(100, RNG.Simple(0))

      result.isFalsified must beFalse
    }
    "fail if both tests fail" in {
      val props =
        Prop("first").forAll(Gen.choose(20, 30)) { i => i >= 10 && i < 20 } ||
          Prop("second").forAll(Gen.choose(150, 250)) { i => i >= 100 && i < 200 }

      val result = props.run(100, RNG.Simple(0))

      result match {
        case Passed => failure("The check shouldn't have been successful.")
        case Falsified(label, failedCase, successes) =>
          failedCase.toInt must beBetween(200, 250).excludingEnd
          label must be some "second"
      }
      success
    }
  }
}

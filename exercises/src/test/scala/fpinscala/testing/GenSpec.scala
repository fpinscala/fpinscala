package fpinscala.testing

import fpinscala.state.RNG
import org.specs2.mutable._

object ScalaCheckGenSpec {
  implicit class GenOps[A](val gen: Gen[A]) extends AnyVal {
    def get(seed: Int): A =
      gen.sample.run(RNG.Simple(seed))._1

  }
}

class ScalaCheckGenSpec extends Specification
  with org.specs2.ScalaCheck {

  import ScalaCheckGenSpec._

  "choose" should {
    "generate numbers in a range" in {
      prop { (seed: Int) =>
        val gen = Gen.choose(10, 20)
        val d = gen.get(seed)
        d must beBetween(10, 20).excludingEnd
      }
    }
  }

  "unit" should {
    "always return the provided value" in {
      prop { (seed: Int, element: Int) =>
        val gen = Gen.unit(element)
        val i = gen.get(seed)
        i === element
      }
    }
  }

  "listOfN" should {
    "return a list of the same item when combined with unit" in {
      prop { (seed: Int, size: Int, element: Int) =>
        val gen = Gen.listOfN(size, Gen.unit(element))
        val list = gen.get(seed)
        list === List.fill(size)(element)
      }.setGen2(org.scalacheck.Gen.posNum[Int])
    }
    "return a list of the different items when combined with choose" in {
      prop { (seed: Int, size: Int, element: Int) =>
        val gen = Gen.listOfN(size, Gen.choose(100, 200))
        val list = gen.get(seed)
        list must have size(size)
        list must contain(beBetween(100, 200).excludingEnd)
      }.setGen2(org.scalacheck.Gen.posNum[Int])
    }
  }

}

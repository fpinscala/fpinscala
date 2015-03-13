package fpinscala.testing.exhaustive

import org.specs2.mutable._

object GenSpec {
  implicit class GenOps[A](val gen: Gen[A]) extends AnyVal {
    def get(seed: Int): A =
      gen.stream(RNG.Simple(seed)).head

  }
}

class GenSpec extends Specification with org.specs2.ScalaCheck {

  import GenSpec._

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
      prop { (seed: Int, size: Int) =>
        val gen = Gen.listOfN(size, Gen.choose(100, 200))
        val list = gen.get(seed)
        list must have size(size)
        list must contain(beBetween(100, 200).excludingEnd)
      }.setGen2(org.scalacheck.Gen.posNum[Int])
    }
    "return a list of the different size if requested." in {
      prop { (seed: Int) =>
        val gen = Gen.choose(100, 200).listOfN(Gen.choose(300, 400))
        val list = gen.get(seed)
        list.length must beBetween(300, 400).excludingEnd
        list must contain(beBetween(100, 200).excludingEnd)
      }
    }
  }

  "union" should {
    "merge two generators" in {
      val seed = 0
      val size = 10000
      val genItem = Gen.unit("abc") union Gen.unit("def")
      val genList = genItem.listOfN(size)

      val list = genList.get(seed)
      val map = list.groupBy(identity).mapValues(_.length).toMap

      map.keys === Set("abc", "def")
      map.values must contain(be ~((size / 2) +/- 100)).forall
    }
  }

  "weighted" should {
    "merge two generators according to weight" in {
      val seed = 0
      val size = 100000
      val genItem = Gen.unit("abc") -> 0.1 union Gen.unit("def") -> 0.9
      val genList = genItem.listOfN(size)

      val list = genList.get(seed)
      val map = list.groupBy(identity).mapValues(_.length).toMap

      map.keys === Set("abc", "def")
      map("abc") must be ~(10000 +/- 100)
      map("def") must be ~(90000 +/- 100)
    }
  }

  "domain" should {
    "be usable to mix with sample" in {
      val seed = 0
      val size = 100000
      val genItem = Gen.unit("abc") -> 0.1 union
        Gen.values("def", "ghi").toSample -> 0.9
      val genList = genItem.listOfN(size)

      val list = genList.get(seed)
      val map = list.groupBy(identity).mapValues(_.length).toMap

      map.keys === Set("abc", "def", "ghi")
      map("abc") must be ~(10000 +/- 200)
      map("def") must be ~(45000 +/- 200)
      map("def") must be ~(45000 +/- 200)
    }
  }
}

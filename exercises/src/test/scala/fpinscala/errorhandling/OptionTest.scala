package fpinscala.errorhandling

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class OptionTest extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {

  "Instance functions" - {
    "should map to None when None" in {
      val result = None.map((str: String) => str.toInt)
      result shouldBe None
    }

    val someMapScenarios = Table[String, String => Number, Number](
      ("input", "fnc", "output"),
      ("12", str => str.toInt, 12L),
      ("123", str => str.toLong, 123L),
      ("12.3", str => str.toDouble, 12.3),
    )

    forAll(someMapScenarios) { (input, fnc, output) =>
      s"should map <$input> to <$output> when Some" in {
        val result = Some(input).map(fnc)
        result shouldBe Some(output)
      }
    }

    "should flatMap to None when None" in {
      val result = None.flatMap((str: String) => Some(str.toInt))
      result shouldBe None
    }

    forAll(someMapScenarios) { (input, fnc, output) =>
      s"should flatMap <$input> to <$output> when Some" in {
        val result = Some(input).flatMap(a => Some(fnc(a)))
        result shouldBe Some(output)
      }
    }

    "should return default value when None" in {
      val result = None.getOrElse(3 + 5)
      result shouldBe 8
    }

    "should return some value when Some" in {
      val result = Some(1).getOrElse(3 / 0)
      result shouldBe 1
    }

    "should return None when default option is None" in {
      val result = None.orElse(None)
      result shouldBe None
    }

    "should return default option when None" in {
      val result = None.orElse(Some(3 + 5))
      result shouldBe Some(8)
    }

    "should return some option value when Some" in {
      val result = Some(1).orElse(Some(3 / 0))
      result shouldBe Some(1)
    }

    "should return None when predicate evaluates false when filter invoked on None" in {
      val result = None.filter(_ => false)
      result shouldBe None
    }

    "should return None when predicate evaluates true when filter invoked on None" in {
      val result = None.filter(_ => true)
      result shouldBe None
    }

    "should return None when predicate evaluates false when filter invoked on Some" in {
      val result = Some("foo").filter(_ => false)
      result shouldBe None
    }

    "should return unchanged Option when predicate evaluates true when filter invoked on Some" in {
      val result = Some("foo").filter(_ => true)
      result shouldBe Some("foo")
    }
  }

  "Companion object functions" - {
    "verify mean" - {
      "should throw when sequence of doubles is null" in {
        an[NullPointerException] should be thrownBy Option.mean(null)
      }

      "should return None when sequence of doubles is empty" in {
        val result = Option.mean(Seq())
        result shouldBe None
      }

      "should return mean when sequence of doubles is not empty" in {
        val result = Option.mean(Seq(2.12, 4.39))
        result shouldBe Some(3.255)
      }
    }

    "verify variance" - {
      "should throw when sequence of doubles is null" in {
        an[NullPointerException] should be thrownBy Option.variance(null)
      }

      "should return None when sequence of doubles is empty" in {
        val result = Option.variance(Seq())
        result shouldBe None
      }

      "should return variance when sequence of doubles is not empty" in {
        val result = Option.variance(Seq(1.0, 2.0, 7.0))
        result shouldBe Some(6.888888888888888)
      }
    }

    "verify map2" - {
      forAll(Table[Option[String], Option[Int]](
        ("Option A", "Option B"),
        (None, None),
        (Some("foo"), None),
        (None, Some(123)),
      )) { (optA, optB) =>
        s"should return None when option A is $optA and when option B is $optB" in {
          val result = Option.map2(optA, optB)((a, b) => a + b)
          result shouldBe None
        }
      }

      "should return Some when both option are Some" in {
        val result = Option.map2(Some("foo"), Some(123))((a, b) => a + b)
        result shouldBe Some("foo123")
      }
    }

    "verify sequence" - {
      forAll(Table[List[Option[String]]](
        "options",
        List(None, None),
        List(Some("foo"), None),
        List(None, Some("bar")),
      )) { options =>
        s"should return None when list of options is $options" in {
          val result1 = Option.sequence(options)
          val result2 = Option.sequenceWithTraverse(options)
          result1 shouldBe None
          result2 shouldBe None
        }
      }

      "should return Some when list of options contains only Somes" in {
        val result1 = Option.sequence(List(Some("foo"), Some("bar")))
        val result2 = Option.sequenceWithTraverse(List(Some("foo"), Some("bar")))
        result1 shouldBe Some(List("foo", "bar"))
        result2 shouldBe Some(List("foo", "bar"))
      }
    }

    "verify traverse" - {
      forAll(Table[List[Int]](
        "ints",
        List(1, 0),
        List(0, 1),
        List(1, 1),
      )) { ints =>
        s"should return None when list of ints is $ints" in {
          val result1 = Option.traverse(ints)(i => if (i == 1) None else Some(i.toString))
          result1 shouldBe None
        }
      }

      "should return Some when transform function only generates Somes" in {
        val result = Option.traverse(List("1", "3"))(str => Some(str.toInt))
        result shouldBe Some(List(1, 3))
      }
    }
  }
}

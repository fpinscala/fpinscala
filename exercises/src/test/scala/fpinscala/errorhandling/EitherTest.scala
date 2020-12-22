package fpinscala.errorhandling

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class EitherTest extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {

  "Instance function" - {
    "should not map underlying value when instance of Left" in {
      val result = Left(1).map(_.toString + "a")
      result shouldBe a[Either[Int, String]]
      result shouldBe Left(1)
    }

    "should map underlying value when instance of Right" in {
      val result = Right(1).map(_.toString + "a")
      result shouldBe Right("1a")
    }

    "should not flatMap underlying value when instance of Left" in {
      val result = Left(1).flatMap(e => Right(e.toString + "a"))
      result shouldBe a[Either[Int, String]]
      result shouldBe Left(1)
    }

    "should return Left with flatMapped underlying value when instance of Right and mapping function returns Left" in {
      val result = Right(1).flatMap(a => Left(a.toString + "a"))
      result shouldBe a[Either[Int, String]]
      result shouldBe Left("1a")
    }

    "should flatMap underlying value when instance of Right and mapping function returns a Right" in {
      val result = Right(1).flatMap(a => Right(a.toString + "a"))
      result shouldBe Right("1a")
    }

    "should return Left when default supplier returns Left when orElse invoked on Left" in {
      val result = Left(1).orElse(Left(3))
      result shouldBe Left(3)
    }

    "should return Right when default supplier returns Right when orElse invoked on Left" in {
      val result = Left(1).orElse(Right(3))
      result shouldBe Right(3)
    }

    "should return original Right when orElse invoked on Right" in {
      val result = Right(1).orElse(Left(3))
      result shouldBe Right(1)
    }

    forAll(Table[Either[Int, Int]](
      "argument",
      Left(3),
      Right(5)
    )) { arg =>
      s"should return Left when map2 invoked on Left with $arg argument" in {
        val result = Left(1).map2(arg)((_, _) => throw new RuntimeException())
        result shouldBe Left(1)
      }
    }

    "should return Left when map2 invoked on Right with Left argument" in {
      val result = Right(1).map2(Left(3))((_, _: Int) => throw new RuntimeException())
      result shouldBe Left(3)
    }

    "should return Right with mapped value when map2 invoked on Right with Right argument" in {
      val result = Right(2).map2(Right(3))(_ * _)
      result shouldBe Right(6)
    }
  }

  "Companion object functions" - {

    "should return Left with first encountered Left value when list contains one or more Left" in {
      val result1 = Either.sequence(List(Right(4), Left("foo"), Left("bar")))
      val result2 = Either.sequenceViaTraverse(List(Right(4), Left("foo"), Left("bar")))
      result1 shouldBe Left("foo")
      result2 shouldBe Left("foo")
    }

    "should return Right with empty list when list is empty" in {
      val result1 = Either.sequence(List())
      val result2 = Either.sequenceViaTraverse(List())
      result1 shouldBe Right(List())
      result2 shouldBe Right(List())
    }

    "should return Right with list of Right values when list contains only Rights" in {
      val result1 = Either.sequence(List(Right(4), Right(8)))
      val result2 = Either.sequenceViaTraverse(List(Right(4), Right(8)))
      result1 shouldBe Right(List(4, 8))
      result2 shouldBe Right(List(4, 8))
    }

    "verify traverse" - {
      forAll(Table[List[String], String](
        ("input", "expectedOutput"),
        (List("a", "b", "c"), "b"),
        (List("a", "c", "b"), "c"),
        (List("b", "c", "a"), "b"),
      )) { (input, expectedOutput) =>
          s"should return Left with $expectedOutput value when input is $input" in {
            val result = Either.traverse(input)(a => if (a == "b" || a == "c") Left(a) else Right(a) )
            result shouldBe Left(expectedOutput)
          }
      }
    }

    "should return Right with empty list when traversing empty list" in {
      val result = Either.traverse(List())(Left(_))
      result shouldBe Right(List())
    }

    "should return Right with list of Right values when list contains only values which maps to Right" in {
      val result = Either.traverse(List(4, 8))(Right(_))
      result shouldBe Right(List(4, 8))
    }
  }
}

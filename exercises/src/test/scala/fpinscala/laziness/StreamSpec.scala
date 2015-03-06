package fpinscala.laziness

import org.specs2.mutable._

class StreamSpec extends Specification {

  "toList" should {
    "create a list out of the stream elements" in {
      Stream(1, 2, 4).toList === List(1, 2, 4)
    }
  }

  type Take = Stream[Int] => Int => Stream[Int]

  def testTake(name: String, take: Take) =
    name should {
      "return a stream that only contains the first n elements" in {
        Stream.ones.take(5).toList === List.fill(5)(1)
      }

      "return the full stream if there's not enough elements" in {
        Stream(1, 2, 5).take(5).toList === List(1, 2, 5)
      }

      "return empty when n is zero" in {
        Stream.ones.take(0) === Empty
      }

      "return empty when called on empty" in {
        Empty.take(0) === Empty
        Empty.take(10) === Empty
      }
    }

  testTake("take", _.take)

  "drop" should {
    "skip the first n elements" in {
      Stream(1, 2, 4).drop(2).toList === List(4)
    }

    "support infinite streams" in {
      Stream.ones.drop(4).take(4).toList === List.fill(4)(1)
    }

    "return empty on an empty stream" in {
      Empty.drop(5) === Empty
    }
  }

  type TakeWhile = Stream[Int] => (Int => Boolean) => Stream[Int]

  def testTakeWhile(name: String, takeWhile: TakeWhile) =
    name should {
      "return elements as long as the predicate match" in {
        takeWhile(Stream(1, 3, 5, 2))(_ % 2 == 1).toList === List(1, 3, 5)
      }

      "return empty if the first element doesn't match the predicate" in {
        takeWhile(Stream(1, 3, 5, 2))(_ % 2 == 0) === Empty
      }

      "return empty when called on empty" in {
        takeWhile(Empty)(_ => true) === Empty
      }
    }

  testTakeWhile("takeWhile", _.takeWhile)

  "forAll" should {
    "return true if all the elements match the predicate" in {
      Stream(1, 3, 5, 9).forAll(_ % 2 == 1) should beTrue
    }

    "return false is any element doesn't match the predicate" in {
      Stream(1, 4, 5, 12).forAll(_ % 2 == 1) should beFalse
    }
    "return true for empty" in {
      Empty.forAll(_ => false) === true
    }
  }

  testTakeWhile("takeWhile_foldRight", _.takeWhile_foldRight)

  "headOption" should {
    "return the first element" in {
      Stream.ones.headOption should be some 1
    }

    "return None if the stream is empty" in {
      Empty.headOption should be none
    }
  }

  type Map = Stream[Int] => (Int => Int) => Stream[Int]

  def testMap(name: String, map: Map) =
    name should {
      "transform a list" in {
        map(Stream(1, 2, 3))(_ * 2).toList === List(2, 4, 6)
      }

      "support infinite streams" in {
        map(Stream.ones)(_ * 2).take(10).toList === List.fill(10)(2)
      }
    }

  testMap("map", _.map)

  "filter" should {
    "remove elements from the list that doesn't match" in {
      Stream(1, 2, 3, 4, 5, 6).filter(_ % 2 == 0).toList === List(2, 4, 6)
    }
    "return empty for an empty stream" in {
      Empty.filter(_ => true) === Empty
    }
    "support infinite streams" in {
      Stream.from(1).filter(_ % 2 == 0).take(4).toList === List(2, 4, 6, 8)
    }
  }

  "append" should {
    "merge streams" in {
      (Stream(1, 2) append Stream(3, 4)).toList === List(1, 2, 3, 4)
    }
    "support infinite streams on the left side" in {
      (Stream.ones append Stream(1)).take(1).toList === List(1)
    }
    "support infinite streams on the right side" in {
      (Stream(1, 2) append Stream.from(3)).take(4).toList === List(1, 2, 3, 4)
    }
    "not read from the right side unless required" in {
      val errord = Stream(1, 2) append { throw new Exception; Stream.from(3) }
      errord.take(2).toList === List(1, 2)
    }
  }

  "flatMap" should {
    "merge returned values" in {
      val suite = Stream(1, 2, 3)
      suite.flatMap(suite.take).toList === List(1, 1, 2, 1, 2, 3)
    }
    "support empty returns" in {
      val suite = Stream(0, 1, 2)
      suite.flatMap(suite.take).toList === List(0, 0, 1)
    }
    "support infinite streams" in {
      Stream.ones.flatMap(Stream(_)).take(5).toList === List.fill(5)(1)
    }
  }

  def testConstants(name: String, constants: String => Stream[String]) =
    name should {
      "return multiple times the same value" in {
        Stream.constants("a").take(100).toList === List.fill(100)("a")
      }
    }

  testConstants("constants", Stream.constants)

  def testFrom(name: String, from: Int => Stream[Int]) =
    name should {
      "should return a serie of number starting with n" in {
        from(0).take(5).toList === List(0, 1, 2, 3, 4)
        from(101).take(5).toList === List(101, 102, 103, 104, 105)
      }
    }


  testFrom("from", Stream.from)

  def testFibs(name: String, fibs: Stream[Int]) =
    name should {
      "return the proper suite of numbers" in {
        fibs.take(10).toList === List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
      }
    }

  testFibs("fibs", Stream.fibs)
  testFibs("fibs_2", Stream.fibs_2)

  // Unfold is tested through fibs, from, constants.

  testFibs("fibs_unfold", Stream.fibs_unfold)
  testFibs("fibs_unfold_2", Stream.fibs_unfold_2)

  testFrom("from_unfold", Stream.from_unfold)
  testFrom("from_unfold", Stream.from_unfold)

  testConstants("constants_unfold", Stream.constants_unfold)

  testMap("map_unfold", _.map_unfold)
  testTake("take_unfold", _.take_unfold)
  testTakeWhile("takeWhile_unfold", _.takeWhile)

  "zipWith" should {
    def ints = Stream.from(0)

    "merge two lists" in {
      val times_2 = (ints zipWith ints)(_ + _)
      times_2.take(5).toList === List(0, 2, 4, 6, 8)
    }
    "support left side shorter" in {
      val times_2 = (ints.take(3) zipWith ints)(_ + _)
      times_2.take(5).toList === List(0, 2, 4)
    }
    "support right side shorter" in {
      val times_2 = (ints zipWith ints.take(2))(_ + _)
      times_2.take(5).toList === List(0, 2)
    }
  }

  "zipAll" should {
    import Stream._
    "merge two streams" in {
      (from(0) zipAll from(1)).take(3).toList === List(
        Some(0) -> Some(1),
        Some(1) -> Some(2),
        Some(2) -> Some(3)
      )
    }
    "stop when both streams end" in {
      (from(0).take(3) zipAll from(1).take(3)).toList === List(
        Some(0) -> Some(1),
        Some(1) -> Some(2),
        Some(2) -> Some(3)
      )
    }
    "pad the right side with None when it ends early" in {
      (from(0).take(2) zipAll from(1).take(3)).toList === List(
        Some(0) -> Some(1),
        Some(1) -> Some(2),
        None -> Some(3)
      )
    }
    "pad the left side with None when it ends early" in {
      (from(0).take(4) zipAll from(1).take(3)).toList === List(
        Some(0) -> Some(1),
        Some(1) -> Some(2),
        Some(2) -> Some(3),
        Some(3) -> None
      )
    }
  }

  type StartsWith = Stream[Int] => Stream[Int] => Boolean

  def testStartsWith(name: String, startsWith: StartsWith) =
    name should {
      import Stream._
      "return true if the right side is the start of the left side" in {
        startsWith(from(0))(from(0).take(5)) should beTrue
      }
      "return true if the right side is empty" in {
        startsWith(from(0))(Empty) should beTrue
      }
      "return false if there's a difference" in {
        startsWith(from(0))(Stream(0, 1, 2, 4)) should beFalse
      }
      "support infinite streams where there's a difference" in {
        startsWith(from(1))(Stream(1, 2) append from(4)) should beFalse
      }
    }

  testStartsWith("startsWith", _.startsWith)
  testStartsWith("startsWith_zipAll_foldRight", _.startsWith_zipAll_foldRight)

  type Tails = Stream[Int] => Stream[Stream[Int]]

  def testTails(name: String, tails: Tails) =
    name should {
      "return the tails of the list" in {
        tails(Stream(1, 2, 3)).toList.map(_.toList) === List(
          List(1, 2, 3),
          List(2, 3),
          List(3),
          Nil
        )
      }

      "support an infinite stream" in {
        tails(Stream.from(1)).map(_.take(3)).take(3)
            .toList.map(_.toList) === List(
          List(1, 2, 3),
          List(2, 3, 4),
          List(3, 4, 5)
        )
      }
    }

  testTails("tails", _.tails)

  "scanRight" should {
    "return the correct aggregate stream" in {
      Stream(1, 2, 3).scanRight(0)(_ + _).toList === List(6, 5, 3, 0)
    }
  }

  testTails("tails_scanRight", _.tails_scanRight)

}
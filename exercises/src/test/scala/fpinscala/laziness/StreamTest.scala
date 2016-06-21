package fpinscala.laziness

import org.scalatest.FunSpec

class StreamTest extends FunSpec {
  trait TestStream {
    var count = 0

    private def incrementCount: Int = {
      count += 1
      count
    }

    def testStream: Stream[Int] = Stream.cons(incrementCount, testStream)
  }

  describe("Stream") {
    it("should only have one empty stream") {
      assertResult(Empty)(Stream())
    }

    it("should be lazy") {
      new TestStream {
        assertResult(0)(count)
      }
    }
  }

  describe("toList") {
    ignore("should convert a stream to a list") {
      val testStream = Stream(1, 2, 3)

      assertResult(List(1,2,3))(testStream.toList)
    }

    ignore("should instantly evaluate all elements of a stream") {
      new TestStream {
        val shortStream = testStream.take(3)
        shortStream.toList

        assertResult(3)(count)
      }
    }

    ignore("should work on an empty list") {
      assertResult(Nil)(Empty.toList)
    }
  }

  describe("take") {
    ignore("should return the first n elements of a stream") {
      new TestStream {
        val shortStream = testStream.take(3)

        assertResult(3)(shortStream.take(3).toList.length)
      }
    }

    ignore("should not evaluate more than one element in the stream") {
      new TestStream {
        val shortStream = testStream.take(3)

        assert(1 >= count)
      }
    }

    ignore("should throw an exception if called on an empty stream") {
      intercept[NoSuchElementException] {
        Empty.take(3)
      }
    }

    ignore("should not throw an exception if called on a small stream") {
      val testStream = Stream(1, 2, 3)

      assert(testStream.take(4).isInstanceOf[Stream[_]])
    }
  }

  describe("drop") {
    ignore("should drop the first n elements of a stream") {
      new TestStream {
        val shortStream = testStream.take(6)

        assertResult(List(3, 4, 5, 6))(shortStream.drop(2).toList)
      }
    }

    ignore("should not evaluate any element in the stream") {
      new TestStream {
        val streamTail = testStream.drop(5)

        assertResult(0)(count)
      }
    }

    ignore("should return an empty stream if called on an empty stream") {
      assertResult(Empty)(Empty.drop(1))
    }

    ignore("should return an empty stream if more than all elements in the stream are dropped") {
      val testStream = Stream(1, 2, 3)

      assertResult(Empty)(testStream.drop(4))
    }
  }

  describe("takeWhile") {
    ignore("should return only elements that match the predicate") {
      new TestStream {
        val evens = testStream.takeWhile(_ < 5)

        assertResult(List(1, 2, 3, 4))(evens.toList)
      }
    }

    ignore("should evaluate only one element in the stream") {
      new TestStream {
        val odds = testStream.takeWhile(_ % 2 != 0)

        assertResult(1)(count)
      }
    }

    ignore("should return an empty stream if called on an empty stream") {
      assertResult(Empty)(Empty.takeWhile(_ => true))
    }
  }

  describe("forAll") {
    ignore("should return true if the predicate is true for all elements") {
      val testStream = Stream(1, 2, 3)

      assertResult(true)(testStream.forAll(_ < 5))
    }

    ignore("should return false if the predicate is false for any element") {
      val testStream = Stream(1, 2, 3)

      assertResult(false)(testStream.forAll(_ != 3))
    }

    ignore("should only evaluate elements until one fails the predicate") {
      new TestStream {
        assertResult(false)(testStream.forAll(_ < 5))
        assertResult(5)(count)
      }
    }
  }

  describe("headOption") {
    ignore("should return Some() for a stream with elements") {
      new TestStream {
        assertResult(Some(1))(testStream.headOption)
      }
    }

    ignore("should return None for a stream with no elements") {
      assertResult(None)(Empty.headOption)
    }

    ignore("should only evaluate one element of the stream") {
      new TestStream {
        val _ = testStream.headOption

        assertResult(1)(count)
      }
    }
  }

  describe("map") {
    ignore("should create a new stream that has the function applied to each element") {
      new TestStream {
        assertResult(List(2, 4, 6))(testStream.map(_ * 2).take(3).toList)
      }
    }

    ignore("should only evaluate one element of the stream") {
      new TestStream {
        testStream.map(_ + 1)
        assertResult(1)(count)
      }
    }
  }

  describe("filter") {
    ignore("should create a new stream containing only those elements that match the predicate") {
      new TestStream {
        assertResult(List(1, 2, 3, 4))(testStream.take(100).filter(_ < 5).toList)
      }
    }

    ignore("should be able to return disjoint parts of a stream") {
      new TestStream {
        assertResult(List(2, 4, 6, 8, 10))(testStream.filter(_ % 2 == 0).take(5).toList)
      }
    }

    ignore("should only evaluate one element of the stream") {
      new TestStream {
        testStream.filter(_ > 0)
        assertResult(1)(count)
      }
    }
  }

  describe("append") {
    ignore("should create a new stream out of the two given streams") {
      val stream1 = Stream(1)
      val stream2 = Stream(2, 3)

      assertResult(List(1, 2, 3))(stream1.append(stream2).toList)
    }

    ignore("should only evaluate one element of the stream") {
      new TestStream {
        val newStream = testStream.append(testStream)

        assertResult(1)(count)
      }
    }

    ignore("should work for empty streams") {
      val fullStream = Stream(1, 2, 3)

      assertResult(List(1, 2, 3))(fullStream.append(Empty).toList)
      assertResult(List(1, 2, 3))(Empty.append(fullStream).toList)
    }
  }

  describe("flatMap") {
    ignore("should create a new stream by applying the given function and flattening the results") {
      new TestStream {
        val newStream = testStream.flatMap(n => Stream(n, n))

        assertResult(List(1, 1, 2, 2))(newStream.take(4).toList)
      }
    }

    ignore("should only evaluate one element of the stream") {
      new TestStream {
        val newStream = testStream.flatMap(n => Stream(n.toString))

        assertResult(1)(count)
      }
    }
  }

  describe("ones") {
    ignore("should be an infinite stream of ones") {
      assert(Stream.ones.take(1000).forAll(_ == 1))
      intercept[StackOverflowError] {
        Stream.ones.toList
      }
    }
  }

  describe("constant") {
    ignore("should create a stream that repeats the same element") {
      val constStream = Stream.constant(1)

      assert(constStream.take(1000).forAll(_ == 1))
    }

    ignore("should work for any type") {
      val intStream = Stream.constant(7)
      val charStream = Stream.constant('f')
      val stringStream = Stream.constant("foobar")
      val doubleStream = Stream.constant(Math.PI)

      assert(intStream.take(1).headOption.get.isInstanceOf[Int])
      assert(intStream.take(1000).forAll(_ == 7))

      assert(charStream.take(1).headOption.get.isInstanceOf[Char])
      assert(charStream.take(1000).forAll(_ == 'f'))

      assert(stringStream.take(1).headOption.get.isInstanceOf[String])
      assert(stringStream.take(1000).forAll(_ == "foobar"))

      assert(doubleStream.take(1).headOption.get.isInstanceOf[Double])
      assert(doubleStream.take(1000).forAll(_ == Math.PI))
    }
  }

  describe("from") {
    ignore("should start at the integer specified") {
      val fromSix = Stream.from(6)

      assert(fromSix.headOption.get == 6)
    }

    ignore("should produce an infinite stream") {
      val fromTwelve = Stream.from(12)

      intercept[StackOverflowError] {
        fromTwelve.toList
      }
    }

    ignore("should increase by one at each index") {
      val fromNegativeFour = Stream.from(-4)

      assertResult(List(-4, -3, -2, -1, 0, 1, 2, 3, 4))(fromNegativeFour.take(9).toList)
    }
  }

  describe("fibs") {
    ignore("should produce the fibonacci sequence") {
      assertResult(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610))(Stream.fibs.take(16).toList)
    }

    ignore("should produce an infinite stream") {
      intercept[StackOverflowError] {
        Stream.fibs.toList
      }
    }
  }

  describe("unfold") {
    ignore("should be able to produce an infinite stream from a state and a function") {
      val fibs = Stream.unfold((0,1)) {
        case (first, second) => Some((first, (second, first + second)))
      }

      assertResult(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610))(fibs.take(16).toList)
      intercept[StackOverflowError] {
        fibs.toList
      }
    }

    ignore("should be able to produce a finite stream if the function returns None") {
      val oneToTen = Stream.unfold(1) { n =>
        if (n <= 10) Some((n, n + 1))
        else None
      }
    }
  }

  describe("zipWith") {
    ignore("should apply a function to each corresponding element in the stream") {
      val constStream = Stream.from(1)

      assertResult(List(2, 4, 6, 8, 10))(constStream.zipWith(constStream)(_ + _).take(5).toList)
      assertResult(List(1, 4, 9, 16, 25))(constStream.zipWith(constStream)(_ * _).take(5).toList)
    }

    ignore("should terminate as soon as either stream terminates") {
      new TestStream {
        val shortStream = Stream.from(1).take(5)

        assertResult(List(1, 2, 3, 4, 5))(testStream.zipWith(shortStream)((a, b) => a).toList)
      }
    }

    ignore("should lazily evaluate") {
      new TestStream {
        val stream2 = Stream.from(1)

        val infiniteStream = testStream.zipWith(stream2)(_ * _)
        assertResult(1)(count)
      }
    }
  }

  describe("zipAll") {
    ignore("should generate tuples of the values in each stream") {
      val testStream = Stream.from(1)

      assertResult(List(
        (Some(1), Some(1)),
        (Some(2), Some(2)),
        (Some(3), Some(3))
      )) {
        (testStream zipAll testStream).take(3).toList
      }
    }

    ignore("should continue as long as either stream has values") {
      new TestStream {
        val threeStream = testStream.take(3)
        val fiveStream = testStream.take(5)
        val zippedStream = threeStream zipAll fiveStream

        assertResult(5)(zippedStream.toList.length)
      }
    }

    ignore("should lazily evaluate") {
      new TestStream {
        val constStream = Stream.from(1)
        val zippedStream = testStream zipAll constStream

        assertResult(1)(count)
      }
    }
  }

  describe("startsWith") {
    ignore("should return true if and only if this stream exactly matches another at the start") {
      new TestStream {
        val matchingStream = Stream.from(1).take(1000)
        val mismatchingStream = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)

        assert(testStream startsWith matchingStream)
        assertResult(false)(testStream startsWith mismatchingStream)
      }
    }

    ignore("should evaluate only what it needs to to determine the result") {
      new TestStream {
        val other = Stream(1, 2, 3, 4, 6)

        testStream startsWith other
        assert(6 >= count)
      }
    }

    ignore("should not be bidirectional") {
      new TestStream {
        val mismatchingStream = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)
        assertResult(false)(mismatchingStream startsWith testStream.take(10))
      }
    }
  }

  describe("tails") {
    ignore("should return successive tails of a stream") {
      val testStream = Stream(1, 2, 3, 4, 5)

      assertResult(List(List(1, 2, 3, 4, 5), List(2, 3, 4, 5), List(3, 4, 5), List(4, 5), List(5), Nil)) {
        (testStream.tails map (_.toList)).toList
      }
    }

    ignore("should operate on infinite streams") {
      new TestStream {
        val tails = testStream.tails

        assert(1 >= count)
      }
    }
  }

  describe("scanRight") {
    ignore("should give a list of all intermediate results of applying a function to a Stream") {
      val shortStream = Stream(1, 2, 3, 4, 5)

      assertResult(List(15, 14, 12, 9, 5, 0))(shortStream.scanRight(0)(_ + _).toList)
    }

    ignore("should work on infinite streams, so long as the intermediate results terminate") {
      val stream = Stream.from(1)

      assertResult(List(false, false, false))(stream.scanRight(true)(_ > 5 && _).take(3).toList)
    }
  }
}

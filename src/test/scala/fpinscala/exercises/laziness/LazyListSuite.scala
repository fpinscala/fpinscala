package fpinscala.exercises.laziness

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.laziness.LazyList
import fpinscala.exercises.laziness.LazyList.*

import scala.util.{Random, Try}

class LazyListSuite extends PropSuite:
  private val genSmallInt = Gen.choose(0, 10)
  private val genMidInt = Gen.choose(0, 100)

  private lazy val genLazyList: Gen[LazyList[Int]] =
    def loop(): Gen[LazyList[Int]] =
      Gen.boolean.flatMap { b =>
        if b then Gen.unit(Empty)
        else
          for {
            head <- Gen.int
            tail <- loop()
          } yield Cons(() => head, () => tail)
      }
    loop()

  test("LazyList.headOption")(genLazyList) {
    case Empty      => assert(Empty.headOption.isEmpty)
    case Cons(h, t) => assert(Cons(h, t).headOption.contains(h()))
  }

  test("LazyList.cons") {
    genLazyList.map(tail => (LazyList.cons(Random.nextInt, tail), Cons(Random.nextInt, () => tail)))
  } { (smartConstructor, oldConstructor) =>
    assertEquals(smartConstructor.headOption, smartConstructor.headOption)
    assertNotEquals(oldConstructor.headOption, oldConstructor.headOption)
  }

  test("LazyList.toList")(genIntList) { list =>
    assertEquals(LazyList(list*).toList, list)
  }

  test("LazyList.take")(genSmallInt ** genLazyList) { case (n, lazyList) =>
    assertEquals(lazyList.take(n).toList, lazyList.toList.take(n))
  }

  test("LazyList.drop")(genSmallInt ** genLazyList) { case (n, lazyList) =>
    assertEquals(lazyList.drop(n).toList, lazyList.toList.drop(n))
  }

  test("LazyList.takeWhile")(genSmallInt ** genLazyList) { case (n, lazyList) =>
    assertEquals(lazyList.takeWhile(_ != n).toList, lazyList.toList.takeWhile(_ != n))
  }

  test("LazyList.forAll")(genSmallInt ** genLazyList) { case (n, lazyList) =>
    assertEquals(lazyList.forAll(_ != n), !lazyList.toList.contains(n))
  }

  /*
  test("LazyList.map")(genSmallInt ** genLazyList) { case (n, lazyList) =>
    assertEquals(lazyList.map(_ + n).toList, lazyList.toList.map(_ + n))
  }

  test("LazyList.filter")(genSmallInt ** genLazyList) { case (n, lazyList) =>
    assertEquals(lazyList.filter(_ != n).toList, lazyList.toList.filter(_ != n))
  }

  test("LazyList.append")(genLazyList ** genLazyList) { case (first, second) =>
    assertEquals(first.append(second).toList, first.toList ++ second.toList)
  }

  test("LazyList.flatMap")(genSmallInt ** genLazyList) { case (n, lazyList) =>
    assertEquals(lazyList.flatMap(a => LazyList(a + n)).toList, lazyList.toList.flatMap(a => List(a + n)))
  }
   */

  test("LazyList.ones")(genMidInt) { n =>
    assertEquals(ones.take(n).toList, List.fill(n)(1))
  }

  test("LazyList.continually")(genMidInt ** genMidInt) { (n, a) =>
    assertEquals(continually(a).take(n).toList, List.fill(n)(a))
  }

  test("LazyList.from")(genMidInt ** genMidInt) { (n, a) =>
    assertEquals(from(a).take(n).toList, (a until (a + n)).toList)
  }

  test("LazyList.fib")(genLengthOfFibonacciSeq) { n =>
    assertEquals(fibs.take(n).toList, theFirst21FibonacciNumbers.take(n).toList)
  }

  test("LazyList.unfold")(genMidInt) { n =>
    val genFirstNumbers: Int => Option[(Int, Int)] =
      m => if m > n then None else Some((m, m + 1))
    assertEquals(unfold(1)(genFirstNumbers).toList, (1 to n).toList)
  }

  test("LazyList.fibsViaUnfold")(genLengthOfFibonacciSeq) { n =>
    assertEquals(fibsViaUnfold.take(n).toList, theFirst21FibonacciNumbers.take(n).toList)
  }

  test("LazyList.fromViaUnfold")(genMidInt ** genMidInt) { (n, a) =>
    assertEquals(fromViaUnfold(a).take(n).toList, (a until (a + n)).toList)
  }

  test("LazyList.continuallyViaUnfold")(genMidInt ** genMidInt) { (n, a) =>
    assertEquals(continuallyViaUnfold(a).take(n).toList, List.fill(n)(a))
  }

  test("LazyList.onesViaUnfold")(genMidInt) { n =>
    assertEquals(onesViaUnfold.take(n).toList, List.fill(n)(1))
  }

  /*
  test("LazyList.mapViaUnfold")(genSmallInt ** genLazyList) { case (n, lazyList) =>
    assertEquals(lazyList.mapViaUnfold(_ + n).toList, lazyList.toList.map(_ + n))
  }

  test("LazyList.takeViaUnfold")(genSmallInt ** genLazyList) { case (n, lazyList) =>
    assertEquals(lazyList.takeViaUnfold(n).toList, lazyList.toList.take(n))
  }

  test("LazyList.takeWhileViaUnfold")(genSmallInt ** genLazyList) { case (n, lazyList) =>
    assertEquals(lazyList.takeWhileViaUnfold(_ != n).toList, lazyList.toList.takeWhile(_ != n))
  }

  test("LazyList.zipWith")(genLazyList ** genLazyList) { case (first, second) =>
    assertEquals(first.zipWith(second)(_ + _).toList, first.toList.zip(second.toList).map(_ + _))
  }

  test("LazyList.zipAll")(genLazyList ** genLazyList) { case (first, second) =>
    assertEquals(first.zipAll(second).toList, first.toList.map(Some(_)).zipAll(second.toList.map(Some(_)), None, None))
  }
   */

  test("LazyList.startsWith")(genLazyList ** genLazyList) { case (list1, list2) =>
    assertEquals(list1.startsWith(list2), list1.toList.startsWith(list2.toList))
    assert(list1.startsWith(empty))
    assert(list1.startsWith(list1))
  }

/*
  test("LazyList.tails")(genLazyList) { lazyList =>
    val list = lazyList.toList
    val expected = (0 to list.length).map(i => list.drop(i)).toList
    assertEquals(lazyList.tails.toList.map(_.toList), expected)
  }

  test("LazyList.hasSubsequence")(genSmallInt ** genLazyList) { case (n, list) =>
    assert(list.hasSubsequence(Empty))
    assert(list.hasSubsequence(list))
    assert(list.hasSubsequence(list.drop(n)))
  }

  test("LazyList.hasSubsequence - random lazy lists")(genLazyList ** genLazyList) { (list1, list2) =>
    assertEquals(list1.hasSubsequence(list2), list1.toList.containsSlice(list2.toList))
  }

  test("LazyList.scanRight")(genLazyList) { lazyList =>
    assertEquals(lazyList.scanRight(0)(_ + _).toList, lazyList.tails.map(_.toList.sum).toList)
    assertEquals(lazyList.scanRight(1)(_ * _).toList, lazyList.tails.map(_.toList.product).toList)
  }
 */

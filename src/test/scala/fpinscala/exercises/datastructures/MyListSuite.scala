package fpinscala.exercises.datastructures

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Gen.`**`
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.{genShortNumber, genDoubleList as genDoubleSList, genIntList as genIntSList}
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.datastructures.MyList
import fpinscala.exercises.datastructures.MyList.{MyNil, Cons}

import scala.util.Try

class MyListSuite extends PropSuite:
  private val genIntList: Gen[MyList[Int]] = genIntSList.map(scalaListToList)
  private val genDoubleList: Gen[MyList[Double]] = genDoubleSList.map(scalaListToList)
  private val genListOfLists: Gen[MyList[MyList[Int]]] =
    for {
      length <- genShortNumber
      slist <- Gen.listOfN(length, genIntList)
    } yield scalaListToList(slist)

  private val genSmallNum: Gen[Int] = Gen.choose(-10, 10)

  test("MyList.tail")(genIntList) {
    case MyNil       => intercept[java.lang.Exception](MyList.tail(MyNil))
    case Cons(x, xs) => assertEquals(MyList.tail(Cons(x, xs)), xs)
  }

  test("MyList.setHead")(genIntList) {
    case MyNil       => intercept[java.lang.Exception](MyList.setHead(MyNil, 0))
    case Cons(x, xs) => assertEquals(MyList.setHead(Cons(x, xs), 0), Cons(0, xs))
  }

  test("MyList.drop")(genIntList ** genSmallNum) { case list ** n =>
    assertEquals(MyList.drop(list, n), scalaListToList(listToScalaList(list).drop(n)))
  }

  test("MyList.dropWhile")(genIntList ** genSmallNum) { case list ** n =>
    val f: Int => Boolean = _ <= n
    assertEquals(MyList.dropWhile(list, f), scalaListToList(listToScalaList(list).dropWhile(f)))
  }

  test("MyList.init")(genIntList) {
    case MyNil => intercept[java.lang.Exception](MyList.init(MyNil))
    case list  => assertEquals(MyList.init(list), scalaListToList(listToScalaList(list).init))
  }

  test("MyList.length")(genIntList) { list =>
    assertEquals(MyList.length(list), listToScalaList(list).length)
  }

  test("MyList.foldLeft")(genIntList) { list =>
    assertEquals(MyList.foldLeft(list, "", _ + _.toString), listToScalaList(list).foldLeft("")(_ + _.toString))
  }

  test("MyList.sumViaFoldLeft")(genIntList) { list =>
    assertEquals(MyList.sumViaFoldLeft(list), listToScalaList(list).sum)
  }

  test("MyList.productViaFoldLeft")(genDoubleList) { list =>
    assertEquals(MyList.productViaFoldLeft(list), listToScalaList(list).product)
  }

  test("MyList.lengthViaFoldLeft")(genIntList) { list =>
    assertEquals(MyList.lengthViaFoldLeft(list), listToScalaList(list).length)
  }

  test("MyList.reverse")(genIntList) { list =>
    assertEquals(MyList.reverse(list), scalaListToList(listToScalaList(list).reverse))
  }

  test("MyList.appendViaFoldRight")(genIntList ** genIntList) { case list1 ** list2 =>
    assertEquals(
      MyList.appendViaFoldRight(list1, list2),
      scalaListToList(listToScalaList(list1) ++ listToScalaList(list2))
    )
  }

  test("MyList.concat")(genListOfLists) { listOfLists =>
    assertEquals(
      MyList.concat(listOfLists),
      scalaListToList(listToScalaList(listOfLists).flatMap(listToScalaList))
    )
  }

  test("MyList.incrementEach")(genIntList) { list =>
    assertEquals(
      MyList.incrementEach(list),
      scalaListToList(listToScalaList(list).map(_ + 1))
    )
  }

  test("MyList.doubleToString")(genDoubleList) { list =>
    assertEquals(
      MyList.doubleToString(list),
      scalaListToList(listToScalaList(list).map(_.toString))
    )
  }

  test("MyList.map")(genIntList) { list =>
    assertEquals(
      MyList.map(list)(_ * 2),
      scalaListToList(listToScalaList(list).map(_ * 2))
    )
  }

  test("MyList.filter")(genIntList) { list =>
    assertEquals(
      MyList.filter(list)(_ % 2 == 0),
      scalaListToList(listToScalaList(list).filter(_ % 2 == 0))
    )
  }

  test("MyList.flatMap")(genIntList) { list =>
    assertEquals(
      MyList.flatMap(list)(a => MyList(a, a)),
      scalaListToList(listToScalaList(list).flatMap(a => List(a, a)))
    )
  }

  test("MyList.addPairwise")(genIntList ** genIntList) { case list1 ** list2 =>
    val expectedSList = listToScalaList(list1).zip(listToScalaList(list2)).map { case (a, b) => a + b }
    assertEquals(MyList.addPairwise(list1, list2), scalaListToList(expectedSList))
  }

  /*
  test("MyList.zipWith")(genIntList ** genIntList) { case list1 ** list2 =>
    val expectedSList = listToScalaList(list1).zip(listToScalaList(list2)).map(_ * _)
    assertEquals(MyList.zipWith(list1, list2, _ * _), scalaListToList(expectedSList))
  }
   */

  test("MyList.hasSubsequence")(genIntList ** genSmallNum) { case list ** n =>
    assert(MyList.hasSubsequence(list, MyNil))
    assert(MyList.hasSubsequence(list, list))
    assert(MyList.hasSubsequence(list, Try(MyList.init(list)).getOrElse(MyNil)))
    assert(MyList.hasSubsequence(list, Try(MyList.tail(list)).getOrElse(MyNil)))
    assert(MyList.hasSubsequence(list, MyList.drop(list, n)))
  }

  test("random lists - MyList.hasSubsequence")(genIntList ** genIntList) { case list1 ** list2 =>
    assertEquals(
      MyList.hasSubsequence(list1, list2),
      listToScalaList(list1).containsSlice(listToScalaList(list2))
    )
  }

  private def listToScalaList[A](list: MyList[A]): List[A] = list match
    case MyNil       => List.empty[A]
    case Cons(x, xs) => x +: listToScalaList(xs)

  private def scalaListToList[A](slist: List[A]): MyList[A] = slist match
    case Nil       => MyNil
    case h :: tail => Cons(h, scalaListToList(tail))

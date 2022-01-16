package fpinscala.exercises.datastructures

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.{genShortNumber, genDoubleList as genDoubleSList, genIntList as genIntSList}
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.datastructures.*
import fpinscala.exercises.datastructures.List.*

import scala.util.Try
import scala.{List as SList, Nil as SNil}

class ListSuite extends PropSuite:
  private val genIntList: Gen[List[Int]] = genIntSList.map(scalaListToList)
  private val genDoubleList: Gen[List[Double]] = genDoubleSList.map(scalaListToList)
  private val genListOfLists: Gen[List[List[Int]]] =
    for {
      length <- genShortNumber
      slist <- Gen.listOfN(length, genIntList)
    } yield scalaListToList(slist)

  private val genSmallNum: Gen[Int] = Gen.choose(-10, 10)

  test("List.tail")(genIntList) {
    case Nil         => intercept[java.lang.Exception](List.tail(Nil))
    case Cons(x, xs) => assertEquals(List.tail(Cons(x, xs)), xs)
  }

  test("List.setHead")(genIntList) {
    case Nil         => intercept[java.lang.Exception](List.setHead(Nil, 0))
    case Cons(x, xs) => assertEquals(List.setHead(Cons(x, xs), 0), Cons(0, xs))
  }

  test("List.drop")(genIntList ** genSmallNum) { (list, n) =>
    assertEquals(List.drop(list, n), scalaListToList(listToScalaList(list).drop(n)))
  }

  test("List.dropWhile")(genIntList ** genSmallNum) { (list, n) =>
    val f: Int => Boolean = _ <= n
    assertEquals(List.dropWhile(list, f), scalaListToList(listToScalaList(list).dropWhile(f)))
  }

  test("List.init")(genIntList) {
    case Nil  => intercept[java.lang.Exception](List.init(Nil))
    case list => assertEquals(List.init(list), scalaListToList(listToScalaList(list).init))
  }

  test("List.length")(genIntList) { list =>
    assertEquals(List.length(list), listToScalaList(list).length)
  }

  test("List.foldLeft")(genIntList) { list =>
    assertEquals(List.foldLeft(list, "", _ + _.toString), listToScalaList(list).foldLeft("")(_ + _.toString))
  }

  test("List.sumViaFoldLeft")(genIntList) { list =>
    assertEquals(List.sumViaFoldLeft(list), listToScalaList(list).sum)
  }

  test("List.productViaFoldLeft")(genDoubleList) { list =>
    assertEquals(List.productViaFoldLeft(list), listToScalaList(list).product)
  }

  test("List.lengthViaFoldLeft")(genIntList) { list =>
    assertEquals(List.lengthViaFoldLeft(list), listToScalaList(list).length)
  }

  test("List.reverse")(genIntList) { list =>
    assertEquals(List.reverse(list), scalaListToList(listToScalaList(list).reverse))
  }

  test("List.appendViaFoldRight")(genIntList ** genIntList) { (list1, list2) =>
    assertEquals(
      List.appendViaFoldRight(list1, list2),
      scalaListToList(listToScalaList(list1) ++ listToScalaList(list2))
    )
  }

  test("List.concat")(genListOfLists) { listOfLists =>
    assertEquals(
      List.concat(listOfLists),
      scalaListToList(listToScalaList(listOfLists).flatMap(listToScalaList))
    )
  }

  test("List.incrementEach")(genIntList) { list =>
    assertEquals(
      List.incrementEach(list),
      scalaListToList(listToScalaList(list).map(_ + 1))
    )
  }

  test("List.doubleToString")(genDoubleList) { list =>
    assertEquals(
      List.doubleToString(list),
      scalaListToList(listToScalaList(list).map(_.toString))
    )
  }

  test("List.map")(genIntList) { list =>
    assertEquals(
      List.map(list)(_ * 2),
      scalaListToList(listToScalaList(list).map(_ * 2))
    )
  }

  test("List.filter")(genIntList) { list =>
    assertEquals(
      List.filter(list)(_ % 2 == 0),
      scalaListToList(listToScalaList(list).filter(_ % 2 == 0))
    )
  }

  test("List.flatMap")(genIntList) { list =>
    assertEquals(
      List.flatMap(list)(a => List(a, a)),
      scalaListToList(listToScalaList(list).flatMap(a => SList(a, a)))
    )
  }

  test("List.addPairwise")(genIntList ** genIntList) { (list1, list2) =>
    val expectedSList = listToScalaList(list1).zip(listToScalaList(list2)).map { case (a, b) => a + b }
    assertEquals(List.addPairwise(list1, list2), scalaListToList(expectedSList))
  }

  /*
  test("List.zipWith")(genIntList ** genIntList) { (list1, list2) =>
    val expectedSList = listToScalaList(list1).zip(listToScalaList(list2)).map(_ * _)
    assertEquals(List.zipWith(list1, list2, _ * _), scalaListToList(expectedSList))
  }
   */

  test("List.hasSubsequence")(genIntList ** genSmallNum) { case (list, n) =>
    assert(List.hasSubsequence(list, Nil))
    assert(List.hasSubsequence(list, list))
    assert(List.hasSubsequence(list, Try(List.init(list)).getOrElse(Nil)))
    assert(List.hasSubsequence(list, Try(List.tail(list)).getOrElse(Nil)))
    assert(List.hasSubsequence(list, List.drop(list, n)))
  }

  test("random lists - List.hasSubsequence")(genIntList ** genIntList) { (list1, list2) =>
    assertEquals(
      List.hasSubsequence(list1, list2),
      listToScalaList(list1).containsSlice(listToScalaList(list2))
    )
  }

  private def listToScalaList[A](list: List[A]): SList[A] = list match
    case Nil         => SList.empty[A]
    case Cons(x, xs) => x +: listToScalaList(xs)

  private def scalaListToList[A](slist: SList[A]): List[A] = slist match
    case SNil      => Nil
    case h :: tail => Cons(h, scalaListToList(tail))

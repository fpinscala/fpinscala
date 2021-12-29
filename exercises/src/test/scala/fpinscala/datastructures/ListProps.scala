package fpinscala.datastructures

import fpinscala.datastructures.List.*
import fpinscala.datastructures.Tree.*
import org.scalacheck.*
import org.scalacheck.Gen.*
import org.scalacheck.Prop.{forAll, propBoolean}

import scala.language.adhocExtensions
import scala.util.Try
import scala.{List as SList, Nil as SNil}

object ListProps extends Properties("fpinscala.datastructures.List"):

  private def listToScalaList[A](list: List[A]): SList[A] = list match
    case List.Nil    => SList.empty[A]
    case Cons(x, xs) => x +: listToScalaList(xs)

  private def scalaListToList[A](slist: SList[A]): List[A] = slist match
    case SNil      => List.Nil
    case h :: tail => List.Cons(h, scalaListToList(tail))

  private val genIntList: Gen[List[Int]] =
    for {
      length <- Gen.choose(0, 10)
      slist <- Gen.listOfN(length, Gen.posNum[Int])
    } yield scalaListToList(slist)

  private val genDoubleList: Gen[List[Double]] =
    for {
      length <- Gen.choose(0, 10)
      slist <- Gen.listOfN(length, Gen.double)
    } yield scalaListToList(slist)

  private val genTwoIntLists =
    for {
      list1 <- genIntList
      list2 <- genIntList
    } yield (list1, list2)

  private val genListOfLists: Gen[List[List[Int]]] =
    for {
      length <- Gen.choose(0, 10)
      slist <- Gen.listOfN(length, genIntList)
    } yield scalaListToList(slist)

  private val genListAndNum =
    for {
      list <- genIntList
      num <- Gen.choose(-10, 10)
    } yield (list, num)

  property("tail") = forAll(genIntList) {
    case List.Nil    => Try(List.tail(List.Nil)).isFailure
    case Cons(x, xs) => List.tail(Cons(x, xs)) == xs
  }

  property("setHead") = forAll(genIntList) {
    case List.Nil    => Try(List.setHead(List.Nil, 0)).isFailure
    case Cons(x, xs) => List.setHead(Cons(x, xs), 0) == Cons(0, xs)
  }

  property("drop") = forAll(genListAndNum) { (list, n) =>
    List.drop(list, n) == scalaListToList(listToScalaList(list).drop(n))
  }

  property("dropWhile") = forAll(genListAndNum) { (list, n) =>
    val f: Int => Boolean = _ <= n
    List.dropWhile(list, f) == scalaListToList(listToScalaList(list).dropWhile(f))
  }

  property("init") = forAll(genIntList) {
    case List.Nil => Try(List.init(List.Nil)).isFailure
    case list     => List.init(list) == scalaListToList(listToScalaList(list).init)
  }

  property("length") = forAll(genIntList) { list =>
    List.length(list) == listToScalaList(list).length
  }

  property("foldLeft") = forAll(genIntList) { list =>
    List.foldLeft(list, "", _ + _.toString) == listToScalaList(list).foldLeft("")(_ + _.toString)
  }

  property("sumViaFoldLeft") = forAll(genIntList) { list =>
    List.sumViaFoldLeft(list) == listToScalaList(list).sum
  }

  property("productViaFoldLeft") = forAll(genDoubleList) { list =>
    List.productViaFoldLeft(list) == listToScalaList(list).product
  }

  property("lengthViaFoldLeft") = forAll(genIntList) { list =>
    List.lengthViaFoldLeft(list) == listToScalaList(list).length
  }

  property("reverse") = forAll(genIntList) { list =>
    List.reverse(list) == scalaListToList(listToScalaList(list).reverse)
  }

  property("appendViaFoldRight") = forAll(genTwoIntLists) { (list1, list2) =>
    List.appendViaFoldRight(list1, list2) == scalaListToList(listToScalaList(list2) ++ listToScalaList(list1))
  }

  property("concat") = forAll(genListOfLists) { listOfLists =>
    List.concat(listOfLists) == scalaListToList(listToScalaList(listOfLists).flatMap(listToScalaList))
  }

  property("incrementEach") = forAll(genIntList) { list =>
    List.incrementEach(list) == scalaListToList(listToScalaList(list).map(_ + 1))
  }

  property("doubleToString") = forAll(genDoubleList) { list =>
    List.doubleToString(list) == scalaListToList(listToScalaList(list).map(_.toString))
  }

  property("map") = forAll(genIntList) { list =>
    List.map(list)(_ * 2) == scalaListToList(listToScalaList(list).map(_ * 2))
  }

  property("filter") = forAll(genIntList) { list =>
    List.filter(list, _ % 2 == 0) == scalaListToList(listToScalaList(list).filter(_ % 2 == 0))
  }

  property("flatMap") = forAll(genIntList) { list =>
    List.flatMap(list, a => List(a, a)) == scalaListToList(listToScalaList(list).flatMap(a => SList(a, a)))
  }

  property("addPairwise") = forAll(genTwoIntLists) { (list1, list2) =>
    val expectedSList = listToScalaList(list1).zip(listToScalaList(list2)).map { case (a, b) => a + b }
    List.addPairwise(list1, list2) == scalaListToList(expectedSList)
  }

  /*
  property("zipWith") = forAll(genTwoIntLists) { (list1, list2) =>
    val expectedSList = listToScalaList(list1).zip(listToScalaList(list2)).map(_ * _)
    List.zipWith(list1, list2, _ * _) == scalaListToList(expectedSList)
  }
   */

  property("hasSubsequence") = forAll(genListAndNum) { case (list, n) =>
    (List.hasSubsequence(list, Nil) :| "must contain Nil") &&
      (List.hasSubsequence(list, list) :| "must contain itself") &&
      (List.hasSubsequence(list, Try(List.init(list)).getOrElse(Nil)) :| "must contain init") &&
      (List.hasSubsequence(list, Try(List.tail(list)).getOrElse(Nil)) :| "must contain tail") &&
      (List.hasSubsequence(list, List.drop(list, n)) :| "must contain its part")
  }

  property("hasSubsequence - random lists") = forAll(genTwoIntLists) { (list1, list2) =>
    List.hasSubsequence(list1, list2) == listToScalaList(list1).containsSlice(listToScalaList(list2))
  }

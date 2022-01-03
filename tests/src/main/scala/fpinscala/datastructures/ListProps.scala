package fpinscala.datastructures

import fpinscala.datastructures.List.*
import fpinscala.testing.exhaustive.*
import fpinscala.testing.exhaustive.Prop.*

import scala.util.Try
import scala.{List as SList, Nil as SNil}

object ListProps:

  private def listToScalaList[A](list: List[A]): SList[A] = list match
    case Nil         => SList.empty[A]
    case Cons(x, xs) => x +: listToScalaList(xs)

  private def scalaListToList[A](slist: SList[A]): List[A] = slist match
    case SNil      => Nil
    case h :: tail => Cons(h, scalaListToList(tail))

  private val genIntList: Gen[List[Int]] =
    for {
      length <- Gen.choose(0, 10)
      slist <- Gen.listOfN(length, Gen.int)
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

  private val tailProp: Prop = forAll(genIntList) {
    case Nil         => Try(List.tail(Nil)).isFailure
    case Cons(x, xs) => List.tail(Cons(x, xs)) == xs
  }.tag("List.tail")

  private val setHeadProp: Prop = forAll(genIntList) {
    case Nil         => Try(List.setHead(Nil, 0)).isFailure
    case Cons(x, xs) => List.setHead(Cons(x, xs), 0) == Cons(0, xs)
  }.tag("List.setHead")

  private val dropProp: Prop = forAll(genListAndNum) { (list, n) =>
    List.drop(list, n) == scalaListToList(listToScalaList(list).drop(n))
  }.tag("List.drop")

  private val dropWhileProp: Prop = forAll(genListAndNum) { (list, n) =>
    val f: Int => Boolean = _ <= n
    List.dropWhile(list, f) == scalaListToList(listToScalaList(list).dropWhile(f))
  }.tag("List.dropWhile")

  private val initProp: Prop = forAll(genIntList) {
    case Nil  => Try(List.init(Nil)).isFailure
    case list => List.init(list) == scalaListToList(listToScalaList(list).init)
  }.tag("List.init")

  private val lengthProp: Prop = forAll(genIntList) { list =>
    List.length(list) == listToScalaList(list).length
  }.tag("List.length")

  private val foldLeftProp: Prop = forAll(genIntList) { list =>
    List.foldLeft(list, "", _ + _.toString) == listToScalaList(list).foldLeft("")(_ + _.toString)
  }.tag("List.foldLeft")

  private val sumViaFoldLeftProp: Prop = forAll(genIntList) { list =>
    List.sumViaFoldLeft(list) == listToScalaList(list).sum
  }.tag("List.sumViaFoldLeft")

  private val productViaFoldLeftProp: Prop = forAll(genDoubleList) { list =>
    List.productViaFoldLeft(list) == listToScalaList(list).product
  }.tag("List.productViaFoldLeft")

  private val lengthViaFoldLeftProp: Prop = forAll(genIntList) { list =>
    List.lengthViaFoldLeft(list) == listToScalaList(list).length
  }.tag("List.lengthViaFoldLeft")

  private val reverseProp: Prop = forAll(genIntList) { list =>
    List.reverse(list) == scalaListToList(listToScalaList(list).reverse)
  }.tag("List.reverse")

  private val appendViaFoldRightProp: Prop = forAll(genTwoIntLists) { (list1, list2) =>
    List.appendViaFoldRight(list1, list2) == scalaListToList(listToScalaList(list1) ++ listToScalaList(list2))
  }.tag("List.appendViaFoldRight")

  private val concatProp: Prop = forAll(genListOfLists) { listOfLists =>
    List.concat(listOfLists) == scalaListToList(listToScalaList(listOfLists).flatMap(listToScalaList))
  }.tag("List.concat")

  private val incrementEachProp: Prop = forAll(genIntList) { list =>
    List.incrementEach(list) == scalaListToList(listToScalaList(list).map(_ + 1))
  }.tag("List.incrementEach")

  private val doubleToStringProp: Prop = forAll(genDoubleList) { list =>
    List.doubleToString(list) == scalaListToList(listToScalaList(list).map(_.toString))
  }.tag("List.doubleToString")

  private val mapProp: Prop = forAll(genIntList) { list =>
    List.map(list)(_ * 2) == scalaListToList(listToScalaList(list).map(_ * 2))
  }.tag("List.map")

  private val filterProp: Prop = forAll(genIntList) { list =>
    List.filter(list)(_ % 2 == 0) == scalaListToList(listToScalaList(list).filter(_ % 2 == 0))
  }.tag("List.filter")

  private val flatMapProp: Prop = forAll(genIntList) { list =>
    List.flatMap(list)(a => List(a, a)) == scalaListToList(listToScalaList(list).flatMap(a => SList(a, a)))
  }.tag("List.flatMap")

  private val addPairwiseProp: Prop = forAll(genTwoIntLists) { (list1, list2) =>
    val expectedSList = listToScalaList(list1).zip(listToScalaList(list2)).map { case (a, b) => a + b }
    List.addPairwise(list1, list2) == scalaListToList(expectedSList)
  }.tag("List.addPairwise")

  private val zipWithProp: Prop = forAll(genTwoIntLists) { (list1, list2) =>
    val expectedSList = listToScalaList(list1).zip(listToScalaList(list2)).map(_ * _)
    List.zipWith(list1, list2, _ * _) == scalaListToList(expectedSList)
  }.tag("List.zipWith")

  private val hasSubsequenceProp: Prop = forAll(genListAndNum) { case (list, n) =>
    List.hasSubsequence(list, Nil) &&
      List.hasSubsequence(list, list) &&
      List.hasSubsequence(list, Try(List.init(list)).getOrElse(Nil)) &&
      List.hasSubsequence(list, Try(List.tail(list)).getOrElse(Nil)) &&
      List.hasSubsequence(list, List.drop(list, n))
  }.tag("List.hasSubsequence")

  private val hasSubsequenceRandomListsProp: Prop = forAll(genTwoIntLists) { (list1, list2) =>
    List.hasSubsequence(list1, list2) == listToScalaList(list1).containsSlice(listToScalaList(list2))
  }.tag("random lists - List.hasSubsequence")

  @main def checkList(): Unit =
    tailProp.run()
    setHeadProp.run()
    dropProp.run()
    dropWhileProp.run()
    initProp.run()
    lengthProp.run()
    foldLeftProp.run()
    sumViaFoldLeftProp.run()
    productViaFoldLeftProp.run()
    lengthViaFoldLeftProp.run()
    reverseProp.run()
    appendViaFoldRightProp.run()
    concatProp.run()
    incrementEachProp.run()
    doubleToStringProp.run()
    mapProp.run()
    filterProp.run()
    flatMapProp.run()
    addPairwiseProp.run()
    zipWithProp.run()
    hasSubsequenceProp.run()
    hasSubsequenceRandomListsProp.run()

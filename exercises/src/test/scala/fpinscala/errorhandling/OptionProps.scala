package fpinscala.errorhandling

import fpinscala.errorhandling.Either.*
import fpinscala.errorhandling.Option.*
import org.scalacheck.*
import org.scalacheck.Gen.*
import org.scalacheck.Prop.{forAll, propBoolean}

import scala.language.adhocExtensions
import scala.{Either as SEither, Left as SLeft, None as SNone, Option as SOption, Right as SRight, Some as SSome}

object OptionProps extends Properties("fpinscala.errorhandling.Option"):
  private val genIntOption: Gen[Option[Int]] =
    for {
      n <- posNum[Int]
      opt <- oneOf(None, Some(n))
    } yield opt

  private val genDoubleList: Gen[List[Double]] =
    for {
      length <- Gen.choose(1, 10)
      genNonEmptyList <- Gen.listOfN(length, Gen.double)
      genList <- Gen.oneOf(List.empty[Double], genNonEmptyList)
    } yield genList

  private val genTwoIntOptions: Gen[(Option[Int], Option[Int])] =
    for {
      opt1 <- genIntOption
      opt2 <- genIntOption
    } yield (opt1, opt2)

  private val genOptionSeq: Gen[List[Option[Int]]] =
    for {
      length <- Gen.choose(0, 10)
      genListWithNone <- Gen.listOfN[Option[Int]](length, Gen.oneOf(Gen.const(None), Gen.posNum[Int].map(Some(_))))
      genList <- Gen.listOfN[Option[Int]](length, Gen.posNum[Int].map(Some(_)))
      genOptList <- Gen.oneOf(List(None), genListWithNone, genList)
    } yield genOptList

  private val genStringList: Gen[List[String]] =
    for {
      length <- Gen.choose(0, 10)
      genListWithRandomString <-
        Gen.listOfN[String](length, Gen.oneOf(Gen.const("one"), Gen.posNum[Int].map(_.toString)))
      genListWithValidNumbers <- Gen.listOfN[String](length, Gen.posNum[Int].map(_.toString))
      genStringList <- Gen.oneOf(genListWithRandomString, genListWithValidNumbers)
    } yield genStringList

  private val intToString: Int => String = a => a.toString
  private val intToOptString: Int => Option[String] = a => Some(a.toString)
  private val strToOptInt: String => Option[Int] = _.toIntOption match
    case SNone        => None
    case SSome(value) => Some(value)

  private val otherOpt: Option[Int] = Some(1)

  property("map") = forAll(genIntOption) {
    case None    => None.map(intToString) == None
    case Some(n) => Some(n).map(intToString) == Some(n.toString)
  }

  property("getOrElse") = forAll(genIntOption) {
    case None    => None.getOrElse(1) == 1
    case Some(n) => Some(n).getOrElse(1) == n
  }

  property("flatMap") = forAll(genIntOption) {
    case None    => None.flatMap(intToOptString) == None
    case Some(n) => Some(n).flatMap(intToOptString) == Some(n.toString)
  }

  property("orElse") = forAll(genIntOption) {
    case None => None.orElse(otherOpt) == otherOpt
    case opt  => opt.orElse(otherOpt) == opt
  }

  property("filter") = forAll(genIntOption) {
    case None => None.filter(a => a == 42) == None
    case Some(n) =>
      Some(n).filter(a => a == n) == Some(n) && Some(n).filter(a => a == n + 1) == None
  }

  property("mean") = forAll(genDoubleList) { list =>
    Option.mean(list) == (if list.isEmpty then None else Some(list.sum / list.length))
  }

  property("variance") = forAll(genDoubleList) { list =>
    val expected =
      if list.isEmpty then None
      else
        val m = list.sum / list.length
        val newList = list.map(x => math.pow(x - m, 2))
        Some(newList.sum / newList.length)
    Option.variance(list) == expected
  }

  property("map2") = forAll(genTwoIntOptions) { (opt1, opt2) =>
    (opt1, opt2) match
      case (Some(a), Some(b)) => Option.map2(opt1, opt2)(_ + _) == Some(a + b)
      case _                  => Option.map2(opt1, opt2)(_ + _) == None
  }

  property("sequence") = forAll(genOptionSeq) { optionList =>
    val expected: Option[List[Int]] =
      if optionList.contains(None) then None
      else Some(optionList.flatMap(_.map(List(_)).getOrElse(List.empty[Int])))
    Option.sequence(optionList) == expected
  }

  property("traverse") = forAll(genStringList) { list =>
    val expected: Option[List[Int]] =
      if list.contains("one") then None
      else Some(list.flatMap(_.toIntOption))
    Option.traverse(list)(strToOptInt) == expected
  }

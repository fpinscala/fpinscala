package fpinscala.tests.errorhandling

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.errorhandling.*
import fpinscala.exercises.errorhandling.Option.*

import scala.{Either as SEither, Left as SLeft, None as SNone, Option as SOption, Right as SRight, Some as SSome}

object OptionProps:
  private val genIntOption: Gen[Option[Int]] =
    Gen.union(Gen.unit(None), Gen.int.map(Some(_)))

  private val genDoubleList: Gen[List[Double]] =
    Gen.choose(0, 10).flatMap(n => Gen.listOfN(n, Gen.double))

  private val genTwoIntOptions: Gen[(Option[Int], Option[Int])] =
    for {
      opt1 <- genIntOption
      opt2 <- genIntOption
    } yield (opt1, opt2)

  private val genNoneSeq: Gen[List[Option[Int]]] = Gen.unit(List(None))

  private val genListWithNone: Gen[List[Option[Int]]] =
    for {
      length <- Gen.choose(0, 10)
      genListWithNone <- Gen.listOfN[Option[Int]](length, genIntOption)
    } yield genListWithNone

  private val genListWithoutNone: Gen[List[Option[Int]]] =
    for {
      length <- Gen.choose(0, 10)
      genList <- Gen.listOfN[Option[Int]](length, Gen.int.map(Some(_)))
    } yield genList

  private val genOptionSeq: Gen[List[Option[Int]]] =
    Gen.union(genNoneSeq, Gen.union(genListWithNone, genListWithoutNone))

  private val genListWithRandomString: Gen[List[String]] =
    Gen.choose(0, 10).flatMap { n =>
      Gen.listOfN(n, Gen.union(Gen.unit("one"), Gen.int.map(_.toString)))
    }

  private val genListWithValidNumbers: Gen[List[String]] =
    Gen.choose(0, 10).flatMap(n => Gen.listOfN(n, Gen.int.map(_.toString)))

  private val genStringList: Gen[List[String]] =
    Gen.union(
      genListWithRandomString,
      genListWithValidNumbers
    )

  private val intToString: Int => String = a => a.toString
  private val intToOptString: Int => Option[String] = a => Some(a.toString)
  private val strToOptInt: String => Option[Int] = _.toIntOption match
    case SNone        => None
    case SSome(value) => Some(value)

  private val otherOpt: Option[Int] = Some(1)

  private val mapProp: Prop = forAll(genIntOption) {
    case None    => None.map(intToString) == None
    case Some(n) => Some(n).map(intToString) == Some(n.toString)
  }.tag("Option.map")

  private val getOrElseProp: Prop = forAll(genIntOption) {
    case None    => None.getOrElse(1) == 1
    case Some(n) => Some(n).getOrElse(1) == n
  }.tag("Option.getOrElse")

  private val flatMapProp: Prop = forAll(genIntOption) {
    case None    => None.flatMap(intToOptString) == None
    case Some(n) => Some(n).flatMap(intToOptString) == Some(n.toString)
  }.tag("Option.flatMap")

  private val orElseProp: Prop = forAll(genIntOption) {
    case None => None.orElse(otherOpt) == otherOpt
    case opt  => opt.orElse(otherOpt) == opt
  }.tag("Option.orElse")

  private val filterProp: Prop = forAll(genIntOption) {
    case None => None.filter(a => a == 42) == None
    case Some(n) =>
      Some(n).filter(a => a == n) == Some(n) && Some(n).filter(a => a == n + 1) == None
  }.tag("Option.filter")

  private val meanProp: Prop = forAll(genDoubleList) { list =>
    Option.mean(list) == (if list.isEmpty then None else Some(list.sum / list.length))
  }.tag("Option.mean")

  private val varianceProp: Prop = forAll(genDoubleList) { list =>
    val expected =
      if list.isEmpty then None
      else
        val m = list.sum / list.length
        val newList = list.map(x => math.pow(x - m, 2))
        Some(newList.sum / newList.length)
    Option.variance(list) == expected
  }.tag("Option.variance")

  private val map2Prop: Prop = forAll(genTwoIntOptions) { (opt1, opt2) =>
    (opt1, opt2) match
      case (Some(a), Some(b)) => Option.map2(opt1, opt2)(_ + _) == Some(a + b)
      case _                  => Option.map2(opt1, opt2)(_ + _) == None
  }.tag("Option.map2")

  private val sequenceProp: Prop = forAll(genOptionSeq) { optionList =>
    val expected: Option[List[Int]] =
      if optionList.contains(None) then None
      else Some(optionList.flatMap(_.map(List(_)).getOrElse(List.empty[Int])))
    Option.sequence(optionList) == expected
  }.tag("Option.sequence")

  private val traverseProp: Prop = forAll(genStringList) { list =>
    val expected: Option[List[Int]] =
      if list.contains("one") then None
      else Some(list.flatMap(_.toIntOption))
    Option.traverse(list)(strToOptInt) == expected
  }.tag("Option.traverse")

  @main def checkOption(): Unit =
    mapProp.run()
    getOrElseProp.run()
    flatMapProp.run()
    orElseProp.run()
    filterProp.run()
    meanProp.run()
    varianceProp.run()
    map2Prop.run()
    sequenceProp.run()
    traverseProp.run()

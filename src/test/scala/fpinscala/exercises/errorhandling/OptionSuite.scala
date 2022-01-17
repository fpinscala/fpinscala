package fpinscala.exercises.errorhandling

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.errorhandling.*
import fpinscala.exercises.errorhandling.Option.*

import scala.{Either as SEither, Left as SLeft, None as SNone, Option as SOption, Right as SRight, Some as SSome}

class OptionSuite extends PropSuite:
  private val genIntOption: Gen[Option[Int]] =
    Gen.union(Gen.unit(None), Gen.int.map(Some(_)))

  private val genNoneSeq: Gen[List[Option[Int]]] =
    Gen.unit(List(None))

  private val genListWithNone: Gen[List[Option[Int]]] =
    genList(genIntOption)

  private val genListWithoutNone: Gen[List[Option[Int]]] =
    genList(Gen.int.map(Some(_)))

  private val genOptionSeq: Gen[List[Option[Int]]] =
    Gen.union(genNoneSeq, Gen.union(genListWithNone, genListWithoutNone))

  private val genListWithRandomString: Gen[List[String]] =
    genList(Gen.union(Gen.unit("one"), Gen.int.map(_.toString)))

  private val genListWithValidNumbers: Gen[List[String]] =
    genList(Gen.int.map(_.toString))

  private val genStringList: Gen[List[String]] =
    Gen.union(genListWithRandomString, genListWithValidNumbers)

  private val intToString: Int => String = a => a.toString
  private val intToOptString: Int => Option[String] = a => Some(a.toString)
  private val strToOptInt: String => Option[Int] =
    _.toIntOption match
      case SNone        => None
      case SSome(value) => Some(value)

  private val otherOpt: Option[Int] = Some(1)

  test("Option.map")(genIntOption) {
    case None    => assertEquals(None.map(intToString), None)
    case Some(n) => assertEquals(Some(n).map(intToString), Some(n.toString))
  }

  test("Option.getOrElse")(genIntOption) {
    case None    => assertEquals(None.getOrElse(1), 1)
    case Some(n) => assertEquals(Some(n).getOrElse(1), n)
  }

  test("Option.flatMap")(genIntOption) {
    case None    => assertEquals(None.flatMap(intToOptString), None)
    case Some(n) => assertEquals(Some(n).flatMap(intToOptString), Some(n.toString))
  }

  test("Option.orElse")(genIntOption) {
    case None => assertEquals(None.orElse(otherOpt), otherOpt)
    case opt  => assertEquals(opt.orElse(otherOpt), opt)
  }

  test("Option.filter")(genIntOption) {
    case None => assertEquals(None.filter(a => a == 42), None)
    case Some(n) =>
      assertEquals(Some(n).filter(a => a == n), Some(n))
      assertEquals(Some(n).filter(a => a == n + 1), None)
  }

  test("Option.mean")(genDoubleList) { list =>
    assertEquals(Option.mean(list), if list.isEmpty then None else Some(list.sum / list.length))
  }

  test("Option.variance")(genDoubleList) { list =>
    val expected =
      if list.isEmpty then None
      else
        val m = list.sum / list.length
        val newList = list.map(x => math.pow(x - m, 2))
        Some(newList.sum / newList.length)
    assertEquals(Option.variance(list), expected)
  }

  test("Option.map2")(genIntOption ** genIntOption) {
    case (Some(a), Some(b)) => assertEquals(Option.map2(Some(a), Some(b))(_ + _), Some(a + b))
    case (opt1, opt2)       => assertEquals(Option.map2(opt1, opt2)(_ + _), None)
  }

  test("Option.sequence")(genOptionSeq) { optionList =>
    val expected: Option[List[Int]] =
      if optionList.contains(None) then None
      else Some(optionList.flatMap(_.map(List(_)).getOrElse(List.empty[Int])))
    assertEquals(Option.sequence(optionList), expected)
  }

  test("Option.traverse")(genStringList) { list =>
    val expected: Option[List[Int]] =
      if list.contains("one") then None
      else Some(list.flatMap(_.toIntOption))
    assertEquals(Option.traverse(list)(strToOptInt), expected)
  }

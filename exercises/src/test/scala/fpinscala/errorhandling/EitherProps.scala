package fpinscala.errorhandling

import fpinscala.errorhandling.Either.{Left, Right, map2All}
import fpinscala.errorhandling.EitherProps.property
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

import scala.language.adhocExtensions

object EitherProps extends Properties("fpinscala.errorhandling.Either"):
  private val genEither: Gen[Either[String, Int]] =
    Gen.oneOf(Gen.asciiStr.map(Left(_)), Gen.posNum[Int].map(Right(_)))

  private val genTwoEither: Gen[(Either[String, Int], Either[String, Int])] =
    for {
      either1 <- genEither
      either2 <- genEither
    } yield (either1, either2)

  property("map") = forAll(genEither) { either =>
    val expected = either match {
      case Left(_)  => either
      case Right(n) => Right(n / 2)
    }
    either.map(_ / 2) == expected
  }

  property("flatMap") = forAll(genEither) { either =>
    val f: Int => Either[String, Int] =
      n => if n % 2 == 0 then Right(n / 2) else Left("An odd number")

    val expected = either match
      case Left(_)                => either
      case Right(n) if n % 2 == 1 => Left("An odd number")
      case Right(n)               => Right(n / 2)

    either.flatMap(f) == expected
  }

  property("orElse") = forAll(genTwoEither) {
    case (Left(l1), either2)  => Left(l1).orElse(either2) == either2
    case (Right(r1), either2) => Right(r1).orElse(either2) == Right(r1)
  }

  case class Name(value: String)
  object Name:
    def make(name: String): Either[String, Name] =
      if name == "" || name == null then Left("Name is empty.")
      else Right(Name(name))

    def make2(name: String): Either[List[String], Name] =
      if name == "" || name == null then Left(List("Name is empty."))
      else Right(Name(name))

  case class Age(value: Int)
  object Age:
    def make(age: Int): Either[String, Age] =
      if age < 0 then Left("Age is out of range.")
      else Right(Age(age))

    def make2(age: Int): Either[List[String], Age] =
      if age < 0 then Left(List("Age is out of range."))
      else Right(Age(age))

  case class Person(name: Name, age: Age)
  object Person:
    def make(name: String, age: Int): Either[String, Person] =
      Name.make(name).map2(Age.make(age))(Person(_, _))

  private val genName: Gen[String] =
    Gen.oneOf(Gen.const(""), Gen.asciiStr)

  private val genAge: Gen[Int] =
    Gen.choose(-50, 50)

  private val genNameAndAge: Gen[(String, Int)] =
    for {
      name <- genName
      age <- genAge
    } yield (name, age)

  property("map2") = forAll(genNameAndAge) { case (name, age) =>
    val expected = (name, age) match {
      case ("", _)         => Left("Name is empty.")
      case (_, n) if n < 0 => Left("Age is out of range.")
      case _               => Right(Person(Name(name), Age(age)))
    }

    Name.make(name).map2(Age.make(age))(Person(_, _)) == expected
  }

  private val genAgesList: Gen[List[Int]] =
    for {
      length <- Gen.choose(0, 10)
      posAgesList <- Gen.listOfN(length, Gen.posNum[Int])
      anyAgesList <- Gen.listOfN(length, genAge)
      ageList <- Gen.oneOf(posAgesList, anyAgesList)
    } yield ageList

  property("traverse") = forAll(genAgesList) { ageList =>
    val expected =
      if ageList.exists(_ < 0) then Left("Age is out of range.")
      else Right(ageList.map(Age(_)))

    Either.traverse(ageList)(Age.make) == expected
  }

  property("sequence") = forAll(genAgesList) { ageList =>
    val expected =
      if ageList.exists(_ < 0) then Left("Age is out of range.")
      else Right(ageList.map(Age(_)))

    Either.sequence(ageList.map(Age.make)) == expected
  }

  property("map2All") = forAll(genNameAndAge) { case (name, age) =>
    val expected = (name, age) match {
      case ("", n) if n < 0 => Left(List("Name is empty.", "Age is out of range."))
      case ("", _)          => Left(List("Name is empty."))
      case (_, n) if n < 0  => Left(List("Age is out of range."))
      case _                => Right(Person(Name(name), Age(age)))
    }

    map2All(Name.make2(name), Age.make2(age), Person(_, _)) == expected
  }

  property("traverseAll") = forAll(genAgesList) { ageList =>
    val negCount = ageList.count(_ < 0)
    val expected =
      if negCount > 0 then Left(List.fill(negCount)("Age is out of range."))
      else Right(ageList.map(Age(_)))

    Either.traverseAll(ageList, Age.make2) == expected
  }

  property("sequenceAll") = forAll(genAgesList) { ageList =>
    val negCount = ageList.count(_ < 0)
    val expected =
      if negCount > 0 then Left(List.fill(negCount)("Age is out of range."))
      else Right(ageList.map(Age(_)))

    Either.sequenceAll(ageList.map(Age.make2)) == expected
  }

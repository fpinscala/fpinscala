package fpinscala.exercises.errorhandling

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.errorhandling.*
import fpinscala.exercises.errorhandling.Either.*

class EitherSuite extends PropSuite:
  private val genEither: Gen[Either[String, Int]] =
    Gen.union(genString.map(Left(_)), Gen.int.map(Right(_)))

  test("Either.map")(genEither) { either =>
    val expected = either match
      case Left(_)  => either
      case Right(n) => Right(n / 2)
    assertEquals(either.map(_ / 2), expected)
  }

  test("Either.flatMap")(genEither) { either =>
    val f: Int => Either[String, Int] =
      n => if n % 2 == 0 then Right(n / 2) else Left("An odd number")

    val expected = either match
      case Left(_)                => either
      case Right(n) if n % 2 != 0 => Left("An odd number")
      case Right(n)               => Right(n / 2)

    assertEquals(either.flatMap(f), expected)
  }

  test("Either.orElse")(genEither ** genEither) {
    case (Left(l1), either2)  => assertEquals(Left(l1).orElse(either2), either2)
    case (Right(r1), either2) => assertEquals(Right(r1).orElse(either2), Right(r1))
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
    Gen.union(Gen.unit(""), genString)

  private val genPosAge: Gen[Int] =
    Gen.choose(1, 50)

  private val genAge: Gen[Int] =
    Gen.choose(-50, 50)

  test("Either.map2")(genName ** genAge) { case (name, age) =>
    val expected = (name, age) match
      case ("", _)         => Left("Name is empty.")
      case (_, n) if n < 0 => Left("Age is out of range.")
      case _               => Right(Person(Name(name), Age(age)))

    assertEquals(Name.make(name).map2(Age.make(age))(Person(_, _)), expected)
  }

  private val genAgesList: Gen[List[Int]] =
    Gen.union(
      Gen.choose(0, 10).flatMap(n => Gen.listOfN(n, genPosAge)),
      Gen.choose(0, 10).flatMap(n => Gen.listOfN(n, genAge))
    )

  test("Either.traverse")(genAgesList) { ageList =>
    val expected =
      if ageList.exists(_ < 0) then Left("Age is out of range.")
      else Right(ageList.map(Age(_)))

    assertEquals(Either.traverse(ageList)(Age.make), expected)
  }

  test("Either.sequence")(genAgesList) { ageList =>
    val expected =
      if ageList.exists(_ < 0) then Left("Age is out of range.")
      else Right(ageList.map(Age(_)))

    assertEquals(Either.sequence(ageList.map(Age.make)), expected)
  }

  test("Either.map2All")(genName ** genAge) { case (name, age) =>
    val expected = (name, age) match
      case ("", n) if n < 0 => Left(List("Name is empty.", "Age is out of range."))
      case ("", _)          => Left(List("Name is empty."))
      case (_, n) if n < 0  => Left(List("Age is out of range."))
      case _                => Right(Person(Name(name), Age(age)))

    assertEquals(map2All(Name.make2(name), Age.make2(age), Person(_, _)), expected)
  }

  test("Either.traverseAll")(genAgesList) { ageList =>
    val negCount = ageList.count(_ < 0)
    val expected =
      if negCount > 0 then Left(List.fill(negCount)("Age is out of range."))
      else Right(ageList.map(Age(_)))

    assertEquals(Either.traverseAll(ageList, Age.make2), expected)
  }

  test("Either.sequenceAll")(genAgesList) { ageList =>
    val negCount = ageList.count(_ < 0)
    val expected =
      if negCount > 0 then Left(List.fill(negCount)("Age is out of range."))
      else Right(ageList.map(Age(_)))

    assertEquals(Either.sequenceAll(ageList.map(Age.make2)), expected)
  }

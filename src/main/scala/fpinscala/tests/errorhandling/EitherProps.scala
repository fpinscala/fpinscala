package fpinscala.tests.errorhandling

import fpinscala.exercises.errorhandling.Either.*
import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*

object EitherProps:
  private val genChar: Gen[Char] =
    Gen.choose(0, 26).map(i => ('a' + i).toChar)

  private val genString: Gen[String] =
    for {
      n <- Gen.choose(0, 20)
      list <- Gen.listOfN(n, genChar)
    } yield list.mkString

  private val genEither: Gen[Either[String, Int]] =
    Gen.union(genString.map(Left(_)), Gen.int.map(Right(_)))

  private val genTwoEither: Gen[(Either[String, Int], Either[String, Int])] =
    for {
      either1 <- genEither
      either2 <- genEither
    } yield (either1, either2)

  private val mapProp: Prop = forAll(genEither) { either =>
    val expected = either match {
      case Left(_)  => either
      case Right(n) => Right(n / 2)
    }
    either.map(_ / 2) == expected
  }.tag("Either.map")

  private val flatMapProp: Prop = forAll(genEither) { either =>
    val f: Int => Either[String, Int] =
      n => if n % 2 == 0 then Right(n / 2) else Left("An odd number")

    val expected = either match
      case Left(_)                => either
      case Right(n) if n % 2 != 0 => Left("An odd number")
      case Right(n)               => Right(n / 2)

    either.flatMap(f) == expected
  }.tag("Either.flatMap")

  private val orElseProp: Prop = forAll(genTwoEither) {
    case (Left(l1), either2)  => Left(l1).orElse(either2) == either2
    case (Right(r1), either2) => Right(r1).orElse(either2) == Right(r1)
  }.tag("Either.orElse")

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

  private val genNameAndAge: Gen[(String, Int)] =
    for {
      name <- genName
      age <- genAge
    } yield (name, age)

  private val map2Prop: Prop = forAll(genNameAndAge) { case (name, age) =>
    val expected = (name, age) match {
      case ("", _)         => Left("Name is empty.")
      case (_, n) if n < 0 => Left("Age is out of range.")
      case _               => Right(Person(Name(name), Age(age)))
    }

    Name.make(name).map2(Age.make(age))(Person(_, _)) == expected
  }.tag("Either.map2")

  private val genAgesList: Gen[List[Int]] =
    Gen.union(
      Gen.choose(0, 10).flatMap(n => Gen.listOfN(n, genPosAge)),
      Gen.choose(0, 10).flatMap(n => Gen.listOfN(n, genAge))
    )

  private val traverseProp: Prop = forAll(genAgesList) { ageList =>
    val expected =
      if ageList.exists(_ < 0) then Left("Age is out of range.")
      else Right(ageList.map(Age(_)))

    Either.traverse(ageList)(Age.make) == expected
  }.tag("Either.traverse")

  private val sequenceProp: Prop = forAll(genAgesList) { ageList =>
    val expected =
      if ageList.exists(_ < 0) then Left("Age is out of range.")
      else Right(ageList.map(Age(_)))

    Either.sequence(ageList.map(Age.make)) == expected
  }.tag("Either.sequence")

  private val map2AllProp: Prop = forAll(genNameAndAge) { case (name, age) =>
    val expected = (name, age) match {
      case ("", n) if n < 0 => Left(List("Name is empty.", "Age is out of range."))
      case ("", _)          => Left(List("Name is empty."))
      case (_, n) if n < 0  => Left(List("Age is out of range."))
      case _                => Right(Person(Name(name), Age(age)))
    }

    map2All(Name.make2(name), Age.make2(age), Person(_, _)) == expected
  }.tag("Either.map2All")

  private val traverseAllProp: Prop = forAll(genAgesList) { ageList =>
    val negCount = ageList.count(_ < 0)
    val expected =
      if negCount > 0 then Left(List.fill(negCount)("Age is out of range."))
      else Right(ageList.map(Age(_)))

    Either.traverseAll(ageList, Age.make2) == expected
  }.tag("Either.traverseAll")

  private val sequenceAllProp: Prop = forAll(genAgesList) { ageList =>
    val negCount = ageList.count(_ < 0)
    val expected =
      if negCount > 0 then Left(List.fill(negCount)("Age is out of range."))
      else Right(ageList.map(Age(_)))

    Either.sequenceAll(ageList.map(Age.make2)) == expected
  }.tag("Either.sequenceAll")

  @main def checkEither(): Unit =
    mapProp.run()
    flatMapProp.run()
    orElseProp.run()
    map2Prop.run()
    traverseProp.run()
    sequenceProp.run()
    map2AllProp.run()
    traverseAllProp.run()
    sequenceAllProp.run()

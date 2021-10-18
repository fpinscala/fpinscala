val nonNegativeInt: Parser[Int] =
  for
    nString <- regex("[0-9]+".r)
    n <- nString.toIntOption match
      case Some(n) => succeed(n)
      case None => fail("expected an integer")
  yield n

val nConsecutiveAs: Parser[Int] = 
  for
    n <- nonNegativeInt
    _ <- char('a').listOfN(n)
  yield n

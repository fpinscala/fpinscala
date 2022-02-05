val nonNegativeInt: Parser[Int] =
  for
    nString <- regex("[0-9]+".r)
    n <- nString.toIntOption match
      case Some(n) => succeed(n)
      case None => fail("expected an integer")
  yield n

val nConsecutiveAs: Parser[String] =
  for
    n <- nonNegativeInt
    s <- char('a').listOfN(n).map(list => s"$n${list.mkString}")
  yield s

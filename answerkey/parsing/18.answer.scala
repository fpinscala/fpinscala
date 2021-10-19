// We'll just give a sketch here. The basic idea is to add an additional field to `ParseError`
case class ParseError(stack: List[(Location,String)] = Nil,
                      otherFailures: List[ParseError] = Nil):

  def addFailure(e: ParseError): ParseError =
    copy(otherFailures = e :: otherFailures)
  ...

// We then need to make sure we populate this in the implementation of `or`
extension [A](p: Parser[A])
  infix def or(p2: => Parser[A]): Parser[A] =
    s => p(s) match
      case Failure(e, false) => p2(s).mapError(_.addFailure(e))
      case r => r // committed failure or success skips running `p2`

// Of course, we have to decide how to print a `ParseError` for human consumption
// We also can expose combinators for selecting which error(s) get reported in the
// event that a chain of `a | b | c` fails--we might choose to collect up all the
// errors for each of the three parsers, or perhaps only show the parser that got
// the furthest in the input before failing, etc

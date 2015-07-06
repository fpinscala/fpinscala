package fpinscala.parsing

import scala.util.matching.Regex

private[parsing] object TestParserTypes {
  type ParseSuccess[+A] = (A, Location)
  type ParseResult[+A] = Either[ParseError, ParseSuccess[A]]
  type Parser[+A] = Location => ParseResult[A]
}

private[parsing] object TestParser extends Parsers[TestParserTypes.Parser] {
  import TestParserTypes._

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input, 0)) match {
      case Right((a, _)) => Right(a)
      case Left(e) => Left(e)
    }
  implicit override def string(s: String): Parser[String] = {
    in =>
      if (input(in).startsWith(s)) Right((s, in.advanceBy(s.length)))
      else parseError(in, s"""string: "${input(in)}" does not start with "$s"""")
  }
  override def succeed[A](a: A): Parser[A] =
    in => Right((a, in))
  implicit override def regex(r: Regex): Parser[String] = {
    in =>
      r.findPrefixOf(input(in)) match {
        case Some(prefix) => Right((prefix, in.advanceBy(prefix.length)))
        case _ => parseError(in, s"""regex: "${input(in)}" does not start with regex "$r"""")
      }
  }
  override def flatMap[A, B](f: Parser[A])(g: A => Parser[B]): Parser[B] = {
    in =>
      f(in) match {
        case Right((a, in1)) => g(a)(in1)
        case left @ Left(_) => left.asInstanceOf[ParseResult[B]]
      }
  }
  override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = {
    in =>
      p1(in) match {
        case left @ Left(_) => p2(in)
        case success => success
      }
  }
  override def slice[A](p: Parser[A]): Parser[String] = {
    def slice(loc: Location, n: Int) = {
      loc.input.substring(loc.offset, math.min(loc.offset + n, loc.input.length))
    }
    in => p(in) match {
      case Right((a, in1)) => Right(slice(in, in1.offset), in1)
      case left @ Left(_) => left.asInstanceOf[ParseResult[String]]
    }
  }
  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s) match {
      case Left(_) => parseError(s, msg)
      case right => right
    }
  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s) match {
      case Left(e) => Left(e.copy(stack = (s, msg) :: e.stack))
      case right => right
    }
  override def attempt[A](p: Parser[A]): Parser[A] = p

  private def input(loc: Location): String = loc.input.substring(loc.offset)
  def parseError[A](loc: Location, msg: String): ParseResult[A] = Left(ParseError(List((loc, msg))))
}

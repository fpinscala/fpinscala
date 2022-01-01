package fpinscala.answers.parsing

import java.util.regex.*
import scala.util.matching.Regex
import fpinscala.answers.testing.*
import language.higherKinds

trait Parsers[Parser[+_]]:

  def string(s: String): Parser[String]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  /*
   * A default `succeed` implementation in terms of `string` and `map`.
   * We leave `succeed` abstract, since `map` is defined below in terms of
   * `flatMap` and `succeed`, which would be a circular definition! But we include
   * the definition here in case implementations wish to use it
   * (say if they provide a custom implementation of `map`, breaking the cycle)
   */
  def defaultSucceed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def succeed[A](a: A): Parser[A]

  def fail(msg: String): Parser[Nothing]

  def regex(r: Regex): Parser[String]

  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = regex("\\s*".r)

  /** Parser which consumes 1 or more digits. */
  def digits: Parser[String] = regex("\\d+".r)

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = regex((".*?" + Pattern.quote(s)).r)

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
    // rather annoying to write, left as an exercise
    // we'll just use quoted (unescaped literals) for now
    quoted.label("string literal").token

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: Parser[String] =
    regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r).token

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString.map(_.toDouble).label("double literal")

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  extension [A](p: Parser[A])

    def run(input: String): Either[ParseError, A]

    infix def or(p2: => Parser[A]): Parser[A]
    def |(p2: => Parser[A]): Parser[A] = p.or(p2)

    def attempt: Parser[A]

    def listOfN(n: Int): Parser[List[A]] =
      if n <= 0 then succeed(Nil)
      else p.map2(p.listOfN(n - 1))(_ :: _)

    def map[B](f: A => B): Parser[B] =
      p.flatMap(f andThen succeed)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      p.product(p2).map((a, b) => f(a, b))

    def many: Parser[List[A]] =
      p.map2(p.many)(_ :: _) | succeed(Nil)

    def many1: Parser[List[A]] =
      p.map2(p.many)(_ :: _)

    def slice: Parser[String]

    def opt: Parser[Option[A]] =
      p.map(Some(_)) | succeed(None)

    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      p.flatMap(a => p2.map(b => (a, b)))

    def **[B](p2: => Parser[B]): Parser[(A,B)] = product(p2)

    def flatMap[B](f: A => Parser[B]): Parser[B]

    def label(msg: String): Parser[A]

    def scope(msg: String): Parser[A]

    /** Sequences two parsers, ignoring the result of the first.
      * We wrap the ignored half in slice, since we don't care about its result.
      */
    def *>[B](p2: => Parser[B]) =
      p.slice.map2(p2)((_, b) => b)

    /** Sequences two parsers, ignoring the result of the second.
      * We wrap the ignored half in slice, since we don't care about its result.
      */
    def <*(p2: => Parser[Any]) =
      p.map2(p2.slice)((a, b) => a)

    /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
    def token: Parser[A] = p.attempt <* whitespace

    /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
    def sep(separator: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
      p.sep1(separator) | succeed(Nil)

    /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
    def sep1(separator: Parser[Any]): Parser[List[A]] =
      p.map2((separator *> p).many)(_ :: _)

    def as[B](b: B): Parser[B] = p.slice.map(_ => b)

    /** Parses a sequence of left-associative binary operators with the same precedence. */
    def opL(op: Parser[(A, A) => A]): Parser[A] =
      p.map2((op ** p).many)((h, t) => t.foldLeft(h)((a, b) => b._1(a, b._2)))

    /** The root of the grammar, expects no further input following `p`. */
    def root: Parser[A] =
      p <* eof

  object Laws:
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => p1.run(s) == p2.run(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
      
end Parsers

case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1

  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match
    case -1 => offset + 1
    case lineStart => offset - lineStart

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  def remaining: String = input.substring(offset)

  def slice(n: Int) = input.substring(offset, offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if input.length > 1
    then
      val itr = input.linesIterator.drop(line - 1)
      if (itr.hasNext) itr.next() else ""
    else ""

  def columnCaret = (" " * (col - 1)) + "^"

case class ParseError(stack: List[(Location, String)] = Nil):
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label(s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latest: Option[(Location,String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest map (_._1)

  /**
  Display collapsed error stack - any adjacent stack elements with the
  same location are combined on one line. For the bottommost error, we
  display the full line, with a caret pointing to the column of the error.
  Example:

  1.1 file 'companies.json'; array
  5.1 object
  5.2 key-value
  5.10 ':'

  { "MSFT" ; 24,
           ^
  */
  override def toString =
    if stack.isEmpty then "no error message"
    else
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
        collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map((loc, msg) => s"${formatLoc(loc)} $msg").mkString("\n") + context

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1).
      view.
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = s"${l.line}.${l.col}"

class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

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


package fpinscala.exercises.parsing

import fpinscala.exercises.parsing.Parsers

import scala.util.matching.Regex

/** This is similar to `fpinscala.answers.parsing.Reference` but using `fpinscala.exercises.parsing.Parsers`.
 *
 * @see
 *   [[fpinscala.answers.parsing.Reference]]
 */
object UnitTestParser extends Parsers[UnitTestParser.Parser]:
  type Parser[+A] = Location => Result[A]

  enum Result[+A]:
    case Success(get: A, length: Int)
    case Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

    def extract: Either[ParseError, A] = this match
      case Failure(e, _) => Left(e)
      case Success(a, _) => Right(a)

    def uncommit: Result[A] = this match
      case Failure(e, true) => Failure(e, false)
      case _                => this

    def addCommit(isCommitted: Boolean): Result[A] = this match
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _             => this

    def mapError(f: ParseError => ParseError): Result[A] = this match
      case Failure(e, c) => Failure(f(e), c)
      case _             => this

    def advanceSuccess(n: Int): Result[A] = this match
      case Success(a, m) => Success(a, n + m)
      case _             => this

  import Result.{Failure, Success}

  def succeed[A](a: A): Parser[A] =
    _ => Success(a, 0)

  def fail(msg: String): Parser[Nothing] =
    l => Failure(l.toError(msg), true)

  /** Returns -1 if s1.startsWith(s2), otherwise returns the first index where the two strings differed. If s2 is longer
   * than s1, returns s1.length.
   */
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int =
    var i = 0
    while (i + offset < s1.length && i < s2.length)
      if s1.charAt(i + offset) != s2.charAt(i) then return i
      i += 1
    if s1.length - offset >= s2.length then -1
    else s1.length - offset

  def string(w: String): Parser[String] =
    l =>
      val i = firstNonmatchingIndex(l.input, w, l.offset)
      if i == -1 then Success(w, w.length)
      else Failure(l.advanceBy(i).toError(s"'$w'"), i != 0)

  def regex(r: Regex): Parser[String] =
    l =>
      r.findPrefixOf(l.remaining) match
        case None    => Failure(l.toError(s"regex $r"), false)
        case Some(m) => Success(m, m.length)

  extension [A](p: Parser[A])

    def run(s: String): Either[ParseError, A] =
      p(Location(s)).extract

    def or(p2: => Parser[A]): Parser[A] =
      l =>
        p(l) match
          case Failure(e, false) => p2(l)
          case r                 => r

    def attempt: Parser[A] = l => p(l).uncommit

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      l =>
        p(l) match
          case Success(a, n) =>
            f(a)(l.advanceBy(n))
              .addCommit(n != 0)
              .advanceSuccess(n)
          case f @ Failure(_, _) => f

    def slice: Parser[String] =
      l =>
        p(l) match
          case Success(a, n)     => Success(l.slice(n), n)
          case f @ Failure(_, _) => f

    def scope(msg: String): Parser[A] =
      l => p(l).mapError(_.push(l, msg))

    def label(msg: String): Parser[A] =
      l => p(l).mapError(_.label(msg))

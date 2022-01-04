package fpinscala.answers
package parsing

import scala.util.matching.Regex

import Reference.Result.{Success, Failure}

object Reference extends Parsers[Reference.Parser]:

  /** A parser is a kind of state action that can fail. */
  // https://github.com/lampepfl/dotty/issues/13761
  /*opaque*/ type Parser[+A] = Location => Result[A]

  enum Result[+A]:
    case Success(get: A, length: Int)
    case Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

    def extract: Either[ParseError, A] = this match
      case Failure(e,_) => Left(e)
      case Success(a,_) => Right(a)

    /* Used by `attempt`. */
    def uncommit: Result[A] = this match
      case Failure(e, true) => Failure(e, false)
      case _ => this

    /* Used by `flatMap`. */
    def addCommit(isCommitted: Boolean): Result[A] = this match
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this

    /* Used by `scope`, `label`. */
    def mapError(f: ParseError => ParseError): Result[A] = this match
      case Failure(e, c) => Failure(f(e), c)
      case _ => this

    def advanceSuccess(n: Int): Result[A] = this match
      case Success(a,m) => Success(a,n+m)
      case _ => this

  // consume no characters and succeed with the given value
  def succeed[A](a: A): Parser[A] =
    _ => Success(a, 0)

  /** Returns -1 if s1.startsWith(s2), otherwise returns the
    * first index where the two strings differed. If s2 is
    * longer than s1, returns s1.length. */
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
      if i == -1 then // they matched
        Success(w, w.length)
      else
        Failure(l.advanceBy(i).toError(s"'$w'"), i != 0)

  /* note, regex matching is 'all-or-nothing':
   * failures are uncommitted */
  def regex(r: Regex): Parser[String] =
    l => r.findPrefixOf(l.remaining) match
      case None => Failure(l.toError(s"regex $r"), false)
      case Some(m) => Success(m, m.length)

  def fail(msg: String): Parser[Nothing] =
    l => Failure(l.toError(msg), true)
  
  extension [A](p: Parser[A])

    def run(s: String): Either[ParseError, A] =
      p(Location(s)).extract

    def or(p2: => Parser[A]): Parser[A] =
      l => p(l) match
        case Failure(e, false) => p2(l)
        case r => r // committed failure or success skips running `p2`

    def attempt: Parser[A] = l => p(l).uncommit

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      l => p(l) match
        case Success(a, n) =>
          f(a)(l.advanceBy(n))
            .addCommit(n != 0)
            .advanceSuccess(n)
        case f @ Failure(_, _) => f

    def slice: Parser[String] =
      l => p(l) match
        case Success(_, n) => Success(l.slice(n), n)
        case f @ Failure(_, _) => f

    /* We provide an overridden version of `many` that accumulates
    * the list of results using a monolithic loop. This avoids
    * stack overflow errors for most grammars.
    */
    override def many: Parser[List[A]] =
      l =>
        var nConsumed: Int = 0
        val buf = new collection.mutable.ListBuffer[A]
        def go(p: Parser[A], offset: Int): Result[List[A]] =
          p(l.advanceBy(offset)) match
            case Success(a, n) =>
              buf += a
              go(p, offset + n)
            case Failure(e, true) => Failure(e, true)
            case Failure(_, _) => Success(buf.toList, offset)
        go(p, 0)

    def scope(msg: String): Parser[A] =
      l => p(l).mapError(_.push(l, msg))

    def label(msg: String): Parser[A] =
      l => p(l).mapError(_.label(msg))


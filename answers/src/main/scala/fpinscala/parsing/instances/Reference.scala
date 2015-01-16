package fpinscala
package parsing

import ReferenceTypes._
import scala.util.matching.Regex

object ReferenceTypes {

  /** A parser is a kind of state action that can fail. */
  type Parser[+A] = ParseState => Result[A]

  /** `ParseState` wraps a `Location` and provides some extra
    * convenience functions. The sliceable parsers defined
    * in `Sliceable.scala` add an `isSliced` `Boolean` flag
    * to `ParseState`.
    */
  case class ParseState(loc: Location) {
    def advanceBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))
    def input: String = loc.input.substring(loc.offset)
    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }

  /* Likewise, we define a few helper functions on `Result`. */
  sealed trait Result[+A] {
    def extract: Either[ParseError,A] = this match {
      case Failure(e,_) => Left(e)
      case Success(a,_) => Right(a)
    }
    /* Used by `attempt`. */
    def uncommit: Result[A] = this match {
      case Failure(e,true) => Failure(e,false)
      case _ => this
    }
    /* Used by `flatMap` */
    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,c) => Failure(e, c || isCommitted)
      case _ => this
    }
    /* Used by `scope`, `label`. */
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e,c) => Failure(f(e),c)
      case _ => this
    }
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a,m) => Success(a,n+m)
      case _ => this
    }
  }
  case class Success[+A](get: A, length: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  /** Returns -1 if s1.startsWith(s2), otherwise returns the
    * first index where the two strings differed. If s2 is
    * longer than s1, returns s1.length. */
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i+offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length-offset >= s2.length) -1
    else s1.length-offset
  }
}

object Reference extends Parsers[Parser] {

  def run[A](p: Parser[A])(s: String): Either[ParseError,A] = {
    val s0 = ParseState(Location(s))
    p(s0).extract
  }

  // consume no characters and succeed with the given value
  def succeed[A](a: A): Parser[A] = s => Success(a, 0)

  def or[A](p: Parser[A], p2: => Parser[A]): Parser[A] =
    s => p(s) match {
      case Failure(e,false) => p2(s)
      case r => r // committed failure or success skips running `p2`
    }

  def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    s => f(s) match {
      case Success(a,n) => g(a)(s.advanceBy(n))
                           .addCommit(n != 0)
                           .advanceSuccess(n)
      case f@Failure(_,_) => f
    }

  def string(w: String): Parser[String] = {
    val msg = "'" + w + "'"
    s => {
      val i = firstNonmatchingIndex(s.loc.input, w, s.loc.offset)
      if (i == -1) // they matched
        Success(w, w.length)
      else
        Failure(s.loc.advanceBy(i).toError(msg), i != 0)
    }
  }

  /* note, regex matching is 'all-or-nothing':
   * failures are uncommitted */
  def regex(r: Regex): Parser[String] = {
    val msg = "regex " + r
    s => r.findPrefixOf(s.input) match {
      case None => Failure(s.loc.toError(msg), false)
      case Some(m) => Success(m,m.length)
    }
  }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s.loc,msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  def fail[A](msg: String): Parser[A] =
    s => Failure(s.loc.toError(msg), true)

  def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  def slice[A](p: Parser[A]): Parser[String] =
    s => p(s) match {
      case Success(_,n) => Success(s.slice(n),n)
      case f@Failure(_,_) => f
    }

  /* We provide an overridden version of `many` that accumulates
   * the list of results using a monolithic loop. This avoids
   * stack overflow errors for most grammars.
   */
  override def many[A](p: Parser[A]): Parser[List[A]] =
    s => {
      var nConsumed: Int = 0
      val buf = new collection.mutable.ListBuffer[A]
      def go(p: Parser[A], offset: Int): Result[List[A]] = {
        p(s.advanceBy(offset)) match {
          case Success(a,n) => buf += a; go(p, offset+n)
          case f@Failure(e,true) => f
          case Failure(e,_) => Success(buf.toList,offset)
        }
      }
      go(p, 0)
    }
}


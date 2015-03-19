package fpinscala.parsing

import java.util.regex._

import scala.util.matching.Regex
import fpinscala.testing.exhaustive._
import fpinscala.testing.exhaustive.Prop._

object Parser extends Parsers[Parser] {

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input, 0)).right.flatMap {
      case (a, loc) =>
        if (loc.tail.isEmpty) Right(a)
        else Left(loc.toError("Trailing text.", true))
    }

  override def count(p: Parser[_]): Parser[Int] = { s1 =>
    p(s1) match {
      case Left(_) => Right(0 -> s1)
      case Right((_, s2)) => count(p)(s2).right.map {
        case (count, s3) => (count + 1) -> s3
      }
    }
  }

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = { loc =>
    s1(loc).left.flatMap { error =>
      if (error.isCommitted) Left(error)
      else s2(loc)
    }
  }

  override def succeed[A](a: A): Parser[A] = { s => Right(a -> s) }

  override implicit def string(text: String): Parser[String] = { loc =>
    if (loc.tail.startsWith(text)) {
      Right(text -> loc.advanceBy(text.length))
    } else {
      Left(loc.toError("Expected: " + text, loc.tail.startsWith(text.headOption.toSeq)))
    }
  }

  override implicit def regex(r: Regex): Parser[String] = { loc =>
    r.findPrefixOf(loc.tail) match {
      case Some(head) => Right(head -> loc.advanceBy(head.length))
      case None => Left(loc.toError("Expected regex: " + r, false))
    }
  }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = { loc1 =>
    p(loc1).right.flatMap {
      case (a, loc2) => f(a)(loc2).left.map(_.addCommit(loc2.offset > loc1.offset))
    }
  }

  override def slice[A](p: Parser[A]): Parser[String] = { loc1 =>
    p(loc1).right.map {
      case (_, loc2) => loc1.tail.substring(0, loc2.offset) -> loc2
    }
  }

  override def attempt[A](p: Parser[A]): Parser[A] = { loc =>
    p(loc).left.map(_.copy(isCommitted = false))
  }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = { loc =>
    p(loc).left.map(_.push(loc, msg))
  }
}

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  implicit def string(s: String): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = f(a)

  def succeed[A](a: A): Parser[A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def count(p: Parser[_]): Parser[Int]

  def product[A, B](s1: Parser[A], s2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- s1
      b <- s2
    } yield (a, b)

  def slice[A](p: Parser[A]): Parser[String]

  def many[A](p: Parser[A]): Parser[List[A]] = many1(p) or succeed(Nil)

  def many1[A](p: Parser[A]): Parser[List[A]] = (p map2 many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case _ if n < 0 => sys.error("Failure...") // FIXME
    case 0 => succeed(Nil)
    case _ => (p map2 listOfN(n - 1, p))(_ :: _)
  }

  implicit def regex(r: Regex): Parser[String]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def attempt[A](p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def values[A](p: Parser[A], sep: Parser[_], scopeName: String = "values"): Parser[List[A]] = scope(scopeName) {
    (p map2 many(sep **> p))(_ :: _) | succeed(Nil)
  }

  implicit class ParserOps[A](val p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def **[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)

    def **>[B](p2: => Parser[B]): Parser[B] =
      self.product(p, p2) map { case (_, b) => b }

    def <**[B](p2: => Parser[B]): Parser[A] =
      self.product(p, p2) map { case (a, _) => a }

    def map[B](f: A => B): Parser[B] = flatMap(f andThen succeed)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      p ** p2 map { case (a, b) => f(a, b) }

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def slice: Parser[String] = self.slice(p)
    def many: Parser[List[A]] = self.many(p)
    def attempt: Parser[A] = self.attempt(p)
  }

  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in) { s => run(p1)(s) == run(p2)(s) }

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)

  }

  object Exercises {
    val exercise96: Parser[String] =
      for {
        d <- raw"\d".r
        as <- listOfN(d.toInt, char('a'))
      } yield d + as
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String, isCommitted: Boolean): ParseError =
    ParseError(List((this, msg)), isCommitted)

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""

  def columnCaret = (" " * (col-1)) + "^"

  def tail = input.substring(offset)
}

case class ParseError(stack: List[(Location,String)], isCommitted: Boolean) {

  def addCommit(commit: Boolean): ParseError =
    copy(isCommitted = isCommitted || commit)

  def push(loc: Location, msg: String): ParseError =
    copy(stack = loc -> msg :: stack)

  override def toString =
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
          collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map { case (loc,msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") +
        context
    }

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location,String)]): List[(Location,String)] =
    s.groupBy(_._1).
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = l.line + "." + l.col

}
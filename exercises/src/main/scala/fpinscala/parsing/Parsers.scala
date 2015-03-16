package fpinscala.parsing

import java.util.regex._

import scala.util.matching.Regex
import fpinscala.testing.exhaustive._
import fpinscala.testing.exhaustive.Prop._

object Parser extends Parsers[Parser] {

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input, 0)).right.flatMap {
      case (a, loc) => if (loc.tail.isEmpty) Right(a) else Left(loc.toError("Trailing text."))
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
      s2(loc) // TODO How to know we're committed and merge errors?
    }
  }

  override def succeed[A](a: A): Parser[A] = { s => Right(a -> s) }

  override implicit def string(text: String): Parser[String] = { loc =>
    if (loc.tail.startsWith(text)) {
      Right(text -> loc.advanceBy(text.length))
    } else {
      Left(loc.toError("Expected: " + text))
    }
  }

  override implicit def regex(r: Regex): Parser[String] = { loc =>
    r.findPrefixOf(loc.tail) match {
      case Some(head) => Right(head -> loc.advanceBy(head.length))
      case None => Left(loc.toError("Expected regex: " + r))
    }
  }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = { loc1 =>
    p(loc1).right.flatMap {
      case (a, loc2) => f(a)(loc2)
    }
  }

  override def slice[A](p: Parser[A]): Parser[String] = ???
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

  implicit class ParserOps[A](val p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def **[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)

    def **>[B](p2: => Parser[B]): Parser[B] =
      self.product(p, p2) map { case (_, b) => b }

    def <**[B](p2: => Parser[B]): Parser[A] =
      self.product(p, p2) map { case (a, _) => a }

    def map[B](f: A => B): Parser[B] = flatMap { a => succeed(f(a)) }

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      p ** p2 map { case (a, b) => f(a, b) }

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def slice: Parser[String] = self.slice(p)
    def many: Parser[List[A]] = self.many(p)
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

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""

  def tail = input.drop(offset)
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
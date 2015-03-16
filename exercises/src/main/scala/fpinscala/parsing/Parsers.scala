package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing.exhaustive._
import fpinscala.testing.exhaustive.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  implicit def string(s: String): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = f(a)

  def succeed[A](a: A): Parser[A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def count(p: Parser[_]): Parser[Int]

  def product[A, B](s1: Parser[A], s2: Parser[B]): Parser[(A, B)] =
    for {
      a <- s1
      b <- s2
    } yield (a, b)

  def slice[A](p: Parser[A]): Parser[String] = ???

  def many[A](p: Parser[A]): Parser[List[A]] = many1(p) or succeed(Nil)

  def many1[A](p: Parser[A]): Parser[List[A]] = (p map2 many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case _ if n < 0 => sys.error("Failure...") // FIXME
    case 0 => succeed(Nil)
    case _ => (p map2 listOfN(n - 1, p))(_ :: _)
  }

  implicit def regex(r: Regex): Parser[String]

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

    def flatMap[B](f: A => Parser[B]): Parser[B] = ???


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
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
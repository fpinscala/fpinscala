package fpinscala.parsing

import scala.util.matching.Regex

import fpinscala.testing.Gen
import fpinscala.testing.Prop
import fpinscala.testing.Prop.forAll

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError,A] // 149, 163

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] // 149, 156
  implicit def string(s: String): Parser[String] // 149
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p) // 150
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = // 150
    ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = // 150, 155
    ???

  def many[A](p: Parser[A]): Parser[List[A]] = // 152, 155
    ???

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = // 152
    ???

  def char(c: Char): Parser[Char] = // 153
    string(c.toString) map ((_: String).charAt(0))

  def succeed[A](a: A): Parser[A] // 153

  def slice[A](p: Parser[A]): Parser[String] // 154

  def many1[A](p: Parser[A]): Parser[List[A]] = // 154
    ???

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = // 154, 156, 157
    ???

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = // 157
    ???

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] // 157

  implicit def regex(r: Regex): Parser[String] // 157

  def label[A](msg: String)(p: Parser[A]): Parser[A] // 161

  def scope[A](msg: String)(p: Parser[A]): Parser[A] // 162

  def attempt[A](p: Parser[A]): Parser[A] // 164

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2) // 150
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2) // 150
    def map[A1 >: A, B](f: A1 => B): Parser[B] = self.map(p)(f) // 152
    def many[B >: A]: Parser[List[B]] = self.many(p) // 152
    def slice: Parser[String] = self.slice(p) // 154
    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2) // 154
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2) // 154
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f) // 157
  }

  object Laws {
  }

  object Exercises {
    def map2ViaProduct[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = // 154
      ???

    def csListOfN[A](p: Parser[A]): Parser[List[A]] = // 157
      ???
  }
}

// 161
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

// 163
case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
  def push(loc: Location, msg: String): ParseError = // 167
    copy(stack = (loc,msg) :: stack)
  def label[A](s: String): ParseError = // 168
    ParseError(latestLoc.map((_, s)).toList)
  def latestLoc: Option[Location] =
    latest map (_._1)
  def latest: Option[(Location, String)] =
    stack.lastOption
}
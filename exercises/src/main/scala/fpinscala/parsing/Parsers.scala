package fpinscala.parsing

import fpinscala.parsing.P.{Failure, ParseState, ParserImpl, Success}
import fpinscala.testing.{Gen, Prop}

import scala.language.higherKinds
import scala.util.matching.Regex

object P {

  case class ParseState(location: Location, isSliced: Boolean = false) {
    def advanceLoc(n: Int) = copy(location = location.advanceBy(n))
    def sliced = copy(isSliced = true)
  }

  type ParserImpl[+A] = ParseState => Result[A]

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, committed = false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, committed) => Failure(e, committed || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, consumed) => Success(a, consumed + n)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure[+A](get: ParseError, committed: Boolean) extends Result[A]
}

object ParsersImpl extends Parsers[P.ParserImpl] {
  override def run[A](p: ParserImpl[A])(input: String): Either[ParseError, A] = p(ParseState(Location(input))) match {
    case Success(get, _) => Right(get)
    case Failure(get, _) => Left(get)
  }

  override def flatMap[A, B](a: ParserImpl[A])(f: A => ParserImpl[B]): ParserImpl[B] = s =>
    a(s) match {
      case Success(get, charsConsumed) =>
        f(get)(s.advanceLoc(charsConsumed))
          .addCommit(charsConsumed > 0)
          .advanceSuccess(charsConsumed)
      case Failure(e, c) => Failure(e, c)
    }

  override implicit def string(str: String): ParserImpl[String] = s => {
    if (s.location.input.substring(s.location.offset).startsWith(str)) Success(str, str.length)
    else Failure(ParseError().push(s.location, s"Expected: $str"), committed = false)
  }

  override def or[A](p1: ParserImpl[A], p2: => ParserImpl[A]): ParserImpl[A] = l => p1(l) match {
    case Failure(_, false) => p2(l)
    case r => r
  }

  override def attempt[A](p: ParserImpl[A]): ParserImpl[A] = s =>
    p(s).uncommit

  override def slice[A](p: ParserImpl[A]): ParserImpl[String] = s =>
    p(s.sliced) match {
      case Success(_, charsConsumed) => Success(s.location.input.substring(s.location.offset, s.location.offset + charsConsumed), charsConsumed)
      case Failure(get, c) => Failure(get, c)
    }

  override implicit def regex(r: Regex): ParserImpl[String] = s =>
    r.findPrefixOf(s.location.input.substring(s.location.offset)) match {
      case Some(str) => Success(str, str.length)
      case None => Failure(ParseError().push(s.location, s"Expected to match: $r"), committed = false)
    }

  override def succeed[A](a: A): ParserImpl[A] = _ => Success(a, 0)

  override def label[A](msg: String)(p: ParserImpl[A]): ParserImpl[A] =
    l => p(l).mapError(_.label(msg))

  override def scope[A](msg: String)(p: ParserImpl[A]): ParserImpl[A] = s =>
    p(s).mapError(_.push(s.location, msg))

  override def furthest[A](p: ParserImpl[A]): ParserImpl[A] = s =>
    p(s).mapError(_.asFarthest)

  override def latest[A](p: ParserImpl[A]): ParserImpl[A] = s =>
    p(s).mapError(_.asLatest)
}

trait Parsers[Parser[+_]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]
  implicit def string(s: String): Parser[String]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def attempt[A](p: Parser[A]): Parser[A]
  def slice[A](p: Parser[A]): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  def succeed[A](a: A): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def furthest[A](p: Parser[A]): Parser[A]
  def latest[A](p: Parser[A]): Parser[A]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(v => succeed(f(v)))

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for {
    r1 <- p1
    r2 <- p2
  } yield f(r1, r2)

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ :: _) | succeed(Nil)
  def listOfN[A](p: Parser[A], n: Int): Parser[List[A]] =
    if (n == 0) succeed(Nil)
    else map2(p, listOfN(p, n - 1))(_ :: _)

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = map2(p1, p2)((_, _))
  def getLeft[A](p1: Parser[A], p2: Parser[Any]): Parser[A] = product(p1, p2) map { case (a, _) => a }
  def getRight[A](p1: Parser[Any], p2: Parser[A]): Parser[A] = product(p1, p2) map { case (_, b) => b }

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ :: _)

  val anyString = regex("[a-zA-Z0-9_ ]+".r)
  val quotedString = "\"" *> anyString <* "\""
  val numbers = regex("-?(0|[1-9]\\d*)(\\.\\d+)?(e-?(0|[1-9]\\d*))?".r)
  val whitespaces = "\\s".r.many
  val eof = regex("\\z".r)

  def manyCharN[A](p: Parser[Char]): Parser[Int] = p.many.slice.map(_.length)
  def many1CharN[A](p: Parser[Char]): Parser[Int] = p.many1.slice.map(_.length)
  def manyPairCharN[A, B](p1: Parser[Char], p2: Parser[Char]): Parser[(Int, Int)] =
    p1.many.slice.map(_.length) ** p2.many1.slice.map(_.length)

  def trim[A](p: Parser[A]): Parser[A] = whitespaces *> p <* whitespaces
  def separated[A](sep: Parser[Any])(p: Parser[A]): Parser[List[A]] = map2(attempt(p <* sep).many, p)(_ :+ _) | succeed(Nil)

  def nChar(p: Parser[Char]): Parser[Int] = for {
    digit <- "\\d".r
    n = digit.toInt
    _ <- listOfN(p, n)
  } yield n

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)

    def many1: Parser[List[A]] = self.many1(p)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def <*(p2: => Parser[Any]): Parser[A] = self.getLeft(p, p2)
    def *>[B](p2: => Parser[B]): Parser[B] = self.getRight(p, p2)

    def trim: Parser[A] = self.trim(p)
    def as[B](b: B): Parser[B] = p.map(_ => b)

  }

  object Laws {

    def equals[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equals(map(p)(identity), p)(in)

    def productLawAssociativity[A](p1: Parser[A], p2: Parser[A], p3: Parser[A])(in: Gen[String]): Prop =
      equals(((p1 ** p2) ** p3) map {
        case ((a, b), c) => a :: b :: c :: Nil
      }, (p1 ** (p2 ** p3)) map {
        case (a, (b, c)) => a :: b :: c :: Nil
      })(in)

    //    def labelLaw[A](p: Parser[A])(inputs: Gen[String], msgs: Gen[String]): Prop =
    //      Prop.forAll(inputs ** msgs) { case (input, msg) =>
    //        run(label(msg)(p))(input) match {
    //          case Left(e) => errorMessage(e) == msg
    //          case _ => true
    //        }
    //      }

    // run(char(c))(c.toString) == Right(c)
    // run(string(s))(s) == Right(s)
    // run(s1 | s2)(s1) == Right(s1)
    // run(s1 | s2)(s2) == Right(s2)
    // run(listOfN(3, s1 | s2)(s1 + s2 + s2) == Right(s1 :: s2 :: s2 :: Nil)


    // run(many(s1)(s1 + s1)) == Right(s1 :: s1 :: Nil)
    // run(many(s1)(s2)) == Right(Nil)
    // map(p)(a => a) == p
    // run(succeed(a)) == Right(a)
    // run(slice(s1)(s1 + s2) == Right(s1)

    // run(product(string(s1), string(s2)))(s1 + s2) == Right((s1, s2))
    // run(p1.map(f) ** p2.map(g))(s) == run((p1 ** p2).map{case {r1, r2} => (f(r1), f(r2)})(s)

  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.linesIterator.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {

  def push(loc: Location, msg: String) = copy(stack = (loc, msg) :: stack)

  def label(msg: String) = ParseError(latestLoc.map((_, msg)).toList)

  def latestLoc: Option[Location] = latest.map(_._1)
  def latest: Option[(Location, String)] = stack.lastOption

  def asLatest: ParseError = latest match {
    case Some((l, m)) => ParseError().push(l, m)
    case None => ParseError()
  }

  def asFarthest: ParseError = stack match {
    case Nil => ParseError()
    case ls =>
      val (l, m) = ls.maxBy(_._1.offset)
      ParseError().push(l, m)
  }

  override def toString: String =
    stack.map({ case (loc, msg) => s"${loc.line}:${loc.col} $msg \n" }).reduce(_ + _)

}

object App extends App {
  println("yo")

  ParsersImpl.run(ParsersImpl.getLeft(ParsersImpl.string("wou"), ParsersImpl.string("yoyo")))("wouyoyo") match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()

  ParsersImpl.run(ParsersImpl.or(ParsersImpl.string("wou"), ParsersImpl.string("yoyo")))("wou") match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()

  ParsersImpl.run(ParsersImpl.or(ParsersImpl.string("wou"), ParsersImpl.string("yoyo")))("yoyo") match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()

  ParsersImpl.run(ParsersImpl.string("\"wou\""))("\"wou\"") match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()

  ParsersImpl.run(ParsersImpl.regex("\\d*".r))("123") match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()

  ParsersImpl.run(ParsersImpl.getLeft(ParsersImpl.getRight(ParsersImpl.string("\""), ParsersImpl.string("yo")), ParsersImpl.string("\"")))("\"yo\"") match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()

  ParsersImpl.run(ParsersImpl.anyString)("sldfjskf") match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()


  import ParsersImpl._


  ParsersImpl.run("\"" *> anyString <* "\"")("\"sldfjskf\"") match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()

  ParsersImpl.run(ParsersImpl.quotedString)("\"sldfjskf\"") match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()

  ParsersImpl.run(JSON.jsonParser(ParsersImpl))("{\"yo\": 123}") match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()

  ParsersImpl.run(JSON.jsonParser(ParsersImpl))("{\"yo\":{}}") match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()

  ParsersImpl.run(JSON.jsonParser(ParsersImpl))(
    s"""{
       |"n" : null,
       |"t" : true,
       |"f" : false,
       |"d": 123123.432,
       |"s": "sdfjlskdfjskldf",
       |"o": {},
       |"o2" : {"yo": 23},
       |"a": [],
       |"a2": [13]
       |}
       |""".stripMargin) match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()

  val jsonTxt =
    """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ],
  "arr" : [[],[]]
}
"""

  ParsersImpl.run(JSON.jsonParser(ParsersImpl))(jsonTxt) match {
    case Left(e) => println(s"Error:\n$e")
    case Right(res) => println(s"Yahoo!\n$res")
  }
  println()

}

object JSON {

  sealed trait JSON
  case object JNull extends JSON
  case class JNumber(n: Double) extends JSON
  case class JString(s: String) extends JSON
  case class JBool(b: Boolean) extends JSON
  case class JObject(props: Map[String, JSON]) extends JSON
  case class JArr(items: IndexedSeq[JSON]) extends JSON

  def jsonParser[P[+_]](p: Parsers[P]): P[JSON] = {
    import p.{asStringParser, eof, numbers, operators, quotedString, separated, scope, attempt}

    implicit def tok(s: String): P[String] = attempt(p.string(s)).trim

    def literals: P[JSON] = scope("literal") {
      "null".as(JNull) |
        "true".as(JBool(true)) |
        "false".as(JBool(false)) |
        numbers.trim.map(_.toDouble).map(JNumber) |
        quotedString.trim.map(JString)
    }

    def v: P[JSON] = literals | obj | array
    def kv: P[(String, JSON)] = (quotedString <* ":") ** v
    def obj: P[JObject] = scope("object")(("{" *> separated(",")(kv) <* "}").map(_.toMap).map(JObject))
    def array: P[JArr] = scope("array")("[" *> separated(",")(v).map(_.toIndexedSeq).map(JArr) <* "]")

    obj <* eof
  }
}


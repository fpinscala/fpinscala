package fpinscala.parsing

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

trait ParserTest[Parser[+_]] {
  val P: Parsers[Parser]
  import P._

  protected def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
      P.run(p)(input)

  protected def limitedStringGen(min: Int, max: Int) =
    Gen.choose(min, max) flatMap { l => Gen.listOfN(l, Gen.alphaNumChar)} map(_.mkString)

  implicit protected val arbStringParser: Arbitrary[Parser[String]] =
//    Arbitrary(arbitrary[String] map string)
    Arbitrary(limitedStringGen(1, 10) map string)

}
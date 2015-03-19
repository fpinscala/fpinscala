package fpinscala.json

import scala.util.parsing.combinator._

sealed trait JsValue
case object JsNull extends JsValue
case class JsBoolean(value: Boolean) extends JsValue
case class JsString(value: String) extends JsValue
case class JsNumber(value: Double) extends JsValue
case class JsArray(items: IndexedSeq[JsValue]) extends JsValue
case class JsObject(items: Map[String, JsValue]) extends JsValue

object JsObject {
  val empty = JsObject()
  def apply(pairs: (String, JsValue)*): JsObject = JsObject(pairs.toMap)
}

private[json] object JsonParser extends JavaTokenParsers {

  def string: Parser[String] = stringLiteral ^^ { s => s.substring(1, s.length - 1) } // Removing quotes
  def pair: Parser[(String, JsValue)] = (string <~ ":") ~ jsValue ^^ { case key ~ value => key -> value }

  def jsNull: Parser[JsNull.type] = "null" ^^ { _ => JsNull }
  def jsBoolean: Parser[JsBoolean] = ("true" | "false") ^^ { s => JsBoolean(s.toBoolean) }
  def jsNumber: Parser[JsNumber] = floatingPointNumber ^^ { s => JsNumber(s.toDouble) }
  def jsString: Parser[JsString] = string ^^ JsString

  def jsArray: Parser[JsArray] = "[" ~> repsep(jsValue, ",") <~ "]" ^^ { values => JsArray(values.toVector) }
  def jsObject: Parser[JsObject] = "{" ~> repsep(pair, ",") <~ "}" ^^ { pairs => JsObject(pairs.toMap) }

  def jsValue: Parser[JsValue] = jsNull | jsBoolean | jsNumber | jsString | jsArray | jsObject

}

object Json {
  def apply(input: String): Either[String, JsValue] = {
    import JsonParser._
    parseAll(jsObject | jsArray, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(msg)
    }
  }
}
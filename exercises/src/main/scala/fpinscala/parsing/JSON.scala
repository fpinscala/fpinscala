package fpinscala.parsing

trait Json

object Json {

  case object JsNull extends Json
  case class JsNumber(get: Double) extends Json
  case class JsString(get: String) extends Json
  case class JsBoolean(get: Boolean) extends Json
  case class JsArray(get: IndexedSeq[Json]) extends Json
  case class JsObject(get: Map[JsString, Json]) extends Json

  object JsObject {
    val empty = JsObject()

    def apply(pairs: (String, Json)*): JsObject = JsObject(pairs.toMap.map { case (key, value) => JsString(key) -> value })
  }

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[Json] = {
    import P._

    val whitespaces: Parser[Unit] = attempt(raw"\s".r.many map { _ => () })

    val escapes: Parser[Char] =
      (char('\\') ** raw"\p{XDigit}{4}".r) map { case (_, hex) => ??? } // \u0000
      Map(
        '"' -> '"',
        '\\' -> '\\',
        '/' -> '/',
        'b' -> '\b',
        'f' -> '\f',
        'n' -> '\n',
        'r' -> '\r',
        't' -> '\t'
      ) map { case (in, out) =>
        (char('\\') ** char(in)) map { _ => out }
      } reduceLeft { _ or _ }

    val nonControl: Parser[Char] = raw"""[^"\\\p{Cntrl}]""".r.map(_.charAt(0))

    def items[A](p: Parser[A]): Parser[List[A]] = values(p, "," ** whitespaces)

    lazy val pair: Parser[(JsString, Json)] = (jsString <** whitespaces ** char(':') ** whitespaces) ** jsValue <** whitespaces
    lazy val pairs: Parser[Map[JsString, Json]] = items(pair).map(_.toMap)

    lazy val jsNull: Parser[JsNull.type] = string("null") map { _ => JsNull }
    lazy val jsBoolean: Parser[JsBoolean] = scope("boolean") {
      (string("true") | string("false")) map { b => JsBoolean(b.toBoolean) }
    }
    lazy val jsNumber: Parser[JsNumber] = scope("number") {
      raw"\d+(\.\d+)?".r map { d => JsNumber(d.toDouble) }
    }
    lazy val jsString: Parser[JsString] = scope("string") {
      char('"') **> many(escapes or nonControl) <** char('"') map {
        case chars => JsString(chars.mkString)
      }
    }

    lazy val jsArray: Parser[JsArray] = scope("array") {
      (char('[') ** whitespaces **> items(jsValue) <** whitespaces ** char(']')) map {
        case items => JsArray(items.toVector)
      }
    }
    lazy val jsObject: Parser[JsObject] = scope("object") {
      char('{') ** whitespaces **> pairs.map(JsObject(_)) <** whitespaces ** char('}')
    }

    lazy val jsValue: Parser[Json] = scope("value") {
      jsNull | jsBoolean | jsNumber | jsString | jsArray | jsObject
    }

    whitespaces **> (jsArray | jsObject) <** whitespaces

  }
}

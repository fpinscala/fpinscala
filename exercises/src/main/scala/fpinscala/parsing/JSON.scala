package fpinscala.parsing

trait Json

object Json {

  case object JsNull extends Json
  case class JsNumber(get: Double) extends Json
  case class JsString(get: String) extends Json
  case class JsBoolean(get: Boolean) extends Json
  case class JsArray(get: IndexedSeq[Json]) extends Json
  case class JsObject(get: Map[JsString, Json]) extends Json

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[Json] = {
    import P._

    val whitespaces = raw"\s".r.slice

    val escapes: Parser[Char] =
      (char('\\') ** raw"[\dA-Fa-F]{4}".r) map { case (_, hex) => ??? } // \u0000
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

    def item[A](p: Parser[A]): Parser[A] = p <** whitespaces ** "," ** whitespaces
    def items[A](p: Parser[A]): Parser[List[A]] = (many(item(p)) map2 p)(_ :+ _) | succeed(Nil)

    def arrayItem: Parser[Json] = item(jsValue)
    def arrayItems: Parser[List[Json]] = items(arrayItem)

    lazy val pair: Parser[(JsString, Json)] = item((jsString <** whitespaces ** char(':') ** whitespaces) ** jsValue)
    lazy val pairs: Parser[Map[JsString, Json]] = items(pair).map(_.toMap)

    lazy val jsNull: Parser[JsNull.type] = string("null") map { _ => JsNull }
    lazy val jsBoolean: Parser[JsBoolean] = (string("true") | string("false")) map { b => JsBoolean(b.toBoolean) }
    lazy val jsNumber: Parser[JsNumber] = raw"\d+(\.\d+)?".r map { d => JsNumber(d.toDouble) }
    lazy val jsString: Parser[JsString] = char('"') **> many(escapes or nonControl) <** char('"') map {
      case chars => JsString(chars.mkString)
    }
    lazy val jsArray: Parser[JsArray] =
      (char('[') ** whitespaces **> arrayItems <** whitespaces ** char(']')) map {
        case items => JsArray(items.toVector)
      }
    lazy val jsObject: Parser[JsObject] = char('{') ** whitespaces **> pairs.map(JsObject) <** whitespaces ** char('}')

    lazy val jsValue: Parser[Json] = jsNull | jsBoolean | jsNumber | jsString | jsArray | jsObject

    whitespaces **> (jsArray | jsObject) <** whitespaces
  }
}

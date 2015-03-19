package fpinscala.json

import org.specs2.mutable._

import JsonParser._

import scala.util.parsing.combinator._

class JsonSpec extends Specification {

  "jsNull" should {
    "parse null" in {
      parseAll(jsNull, "null") match {
        case Success(result, _) => result === JsNull
        case _ => failure
      }
      success
    }
  }

  "Json" should {

    "parse an empty object" in {
      Json("{}") === Right(JsObject.empty)
      Json("{ }") === Right(JsObject.empty)
      Json("{\t\n}") === Right(JsObject.empty)
    }

    "parse a trivial object" in {

      val expected = JsObject(
        "key1" -> JsNumber(1),
        "key2" -> JsString("value")
      )

      val text =
        """
          |{
          |  "key1" : 1 ,
          |  "key2": "value"
          |}
        """.stripMargin

      Json(text) === Right(expected)
    }

    "parse a simple array" in {

      val expected = JsArray(Vector(
        JsNumber(1),
        JsString("value")
      ))

      val text =
        """
          |[ 1, "value" ]
        """.stripMargin

      Json(text) === Right(expected)
    }


    "parse a more complex mix" in {

      val expected = JsArray(Vector(
        JsNumber(1),
        JsString("value"),
        JsNull,
        JsObject(
          "key1" -> JsBoolean(true),
          "key2" -> JsBoolean(false)
        )
      ))

      val text =
        """
          |[
          |  1,
          |  "value",
          |  null,
          |  {
          |    "key1": true,
          |    "key2": false
          |  }
          |]
        """.stripMargin

      Json(text) === Right(expected)
    }

  }

}

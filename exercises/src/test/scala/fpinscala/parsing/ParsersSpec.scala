package fpinscala.parsing

import org.specs2.mutable._

import Json._
import Parser._

class ParsersSpec extends Specification {

  "Parser" should {
    "parse a simple string" in {
      val parser = string("hello")

      run(parser)("hello") should beRight("hello")
      run(parser)("World") should beLeft
    }

    "parse a simple regex" in {
      val parser = regex("hello".r)

      run(parser)("hello") should beRight("hello")
      run(parser)("World") should beLeft
    }

    "parse one or more hello" in {
      val parser = string("hello").many

      run(parser)("hellohello") should beRight(List("hello", "hello"))
      run(parser)("World") should beLeft
    }
  }

  "Json" should {

    val parser = jsonParser(Parser)

    "parse an empty object" in {
      run(parser)("{}") === Right(JsObject.empty)
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

      run(parser)(text) === Right(expected)
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

      run(parser)(text) === Right(expected)
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

      run(parser)(text) === Right(expected)
    }

  }

}


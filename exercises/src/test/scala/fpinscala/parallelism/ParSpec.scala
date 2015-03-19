package fpinscala.parallelism

import java.util.concurrent.{TimeUnit, Executors}

import org.specs2.mutable._
import org.specs2.specification.AfterAll

import Par._

class ParSpec extends Specification with AfterAll {

  val es = Executors.newCachedThreadPool()

  override def afterAll(): Unit = es.shutdown()

  "sum" should {
    "return the sum of all the ints" in {
      val sum = Examples.sum(Vector(1, 2, 3, 4, 5))(es).get(1, TimeUnit.SECONDS)

      sum === 15
    }
  }

  "sum_mapReduce" should {
    "return the sum of all the ints" in {
      val sum = Examples.sum_mapReduce(Vector(1, 2, 3, 4, 5))(es).get(1, TimeUnit.SECONDS)

      sum === 15
    }
  }

  "sum_reduce" should {
    "return the sum of all the ints" in {
      val sum = Examples.sum_reduce(Vector(1, 2, 3, 4, 5))(es).get(1, TimeUnit.SECONDS)

      sum === 15
    }
  }

  "max_reduce" should {
    "return the max of all the ints" in {
      val max = Examples.max(Vector(1, 2, 3, 4, 5))(es).get

      max === Some(5)
    }

    "return the max of all the ints" in {
      val max = Examples.max(Vector(1, 200, -3, 4, 5))(es).get

      max === Some(200)
    }

    "return none for empty list" in {
      val max = Examples.max(Vector.empty)(es).get

      max === None
    }
  }

  "asyncF" should {
    "execute async" in {
      val toPar = asyncF { (a: Int) => a * 2 }

      toPar(10)(es).get == 20
    }
  }

  "parMap" should {
    "transform a list asynchronously" in {
      val par = parMap(List(1, 2, 3))(_ * 2)

      par(es).get == List(2, 4, 6)
    }
  }

  "parFilter" should {
    "remove elements that doesn't match the predicate asynchronously" in {
      val par = parFilter(List(1, 2, 4, 6, 7))(_ % 2 == 1)

      par(es).get === List(1, 7)
    }
  }

  "parFilter_mapReduce" should {
    "remove elements that doesn't match the predicate asynchronously" in {
      val par = parFilter_mapReduce(Vector(1, 2, 4, 6, 7))(_ % 2 == 1)

      par(es).get === List(1, 7)
    }
  }

  "wordCountForParagraphs" should {
    "return zero for an empty list" in {
      wordCountForParagraphs(Vector.empty)(es).get === 0
    }
    "return zero for an empty string" in {
      wordCountForParagraphs(Vector(""))(es).get === 0
    }
    "return the word count" in {
      val words = Vector("Hello, World.", "How is it going?")
      wordCountForParagraphs(words)(es).get === 6
    }
  }

  "wordCount" should {
    "return the number of word in a string" in {
      val p = "Hello, World. How is it going?"
      wordCount(p) === 6
    }
    "return 0 for an empty string" in {
      wordCount("") === 0
    }
  }
}

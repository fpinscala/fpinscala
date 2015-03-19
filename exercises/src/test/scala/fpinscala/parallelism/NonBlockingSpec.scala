package fpinscala.parallelism

import java.util.concurrent.Executors

import org.specs2.mutable._
import org.specs2.specification.AfterAll

import Nonblocking._
import Nonblocking.Par._

import scala.util.Try

class NonBlockingSpec extends Specification with AfterAll {

  val es = Executors.newSingleThreadExecutor

  override def afterAll(): Unit = es.shutdown()

  "parMap" should {
    "work" in {
      val p = parMap(List.range(1, 10000))(Math.sqrt(_))

      val list = run(es)(p)

      list should have size(10000 - 1)
      list(0) should beEqualTo (1.0)
      list(8) should beEqualTo (3.0)
    }
  }

  "fork" should {
    "support exceptions" in  {
      val p = fork {
        sys.error("Failure test")
        unit("abc")
      }
      Try(run(es)(p)) should beAFailedTry
    }
  }

  "lazyUnit" should {
    "support exceptions" in {
      val p = fork {
        lazyUnit {
          sys.error("Failure test")
          "abc"
        }
      }
      Try(run(es)(p)) should beAFailedTry
    }
  }

  "choice" should {
    "return the first par when the condition is true" in {
      val one = choice(unit(true))(unit(1), unit(2))
      run(es)(one) === 1
    }
    "return the second par when the condition is false" in {
      val one = choice(unit(false))(unit(1), unit(2))
      run(es)(one) === 2
    }
  }

  "choiceN" should {
    "return the element identified by the index" in {
      val list = (1 to 10).map(unit).toList
      val pick = choiceN(unit(3))(list)
      run(es)(pick) === 4
    }
  }

  "choiceViaChoiceN" should {
    "return the first par when the condition is true" in {
      val one = choiceViaChoiceN(unit(true))(unit(1), unit(2))
      run(es)(one) === 1
    }
    "return the second par when the condition is false" in {
      val one = choiceViaChoiceN(unit(false))(unit(1), unit(2))
      run(es)(one) === 2
    }
  }

  "choiceNChooser" should {
    "return the element identified by the index" in {
      val list = (1 to 10).map(unit).toList
      val pick = choiceNChooser(unit(3))(list)
      run(es)(pick) === 4
    }
  }

  "choiceViaChooser" should {
    "return the first par when the condition is true" in {
      val one = choiceViaChooser(unit(true))(unit(1), unit(2))
      run(es)(one) === 1
    }
    "return the second par when the condition is false" in {
      val one = choiceViaChooser(unit(false))(unit(1), unit(2))
      run(es)(one) === 2
    }
  }

  "join" should {
    "flatten a par" in {
      val x: Int = run(es)(join(unit(unit(2))))
      x === 2
    }
  }

  "flatten" should {
    "flatten a par" in {
      val x: Int = run(es)(unit(unit(2)).flatten)
      x === 2
    }
  }

}

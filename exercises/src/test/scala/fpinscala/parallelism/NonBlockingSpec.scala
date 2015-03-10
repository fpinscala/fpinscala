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

}

package fpinscala.gettingstarted

import fpinscala.gettingstarted.MyModule.fib
import org.scalatest.{FunSpec, Matchers}
import org.scalatest.prop.TableDrivenPropertyChecks._

class TestFibSpec extends FunSpec with Matchers {
  describe("Exercise 2.1: fib()") {
    it("should generate valid fibonacci numbers") {
      val fibonacciAssertions = Table(
        ("given", "result"),
        (0, 0),
        (1, 1),
        (2, 1),
        (3, 2),
        (4, 3),
        (5, 5),
        (6, 8),
        (7, 13),
        (8, 21))

      forAll(fibonacciAssertions) {
        (value, result) => fib(value) shouldBe result
      }
    }
  }
}

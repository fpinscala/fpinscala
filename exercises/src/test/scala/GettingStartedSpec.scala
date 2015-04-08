import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.gettingstarted._

class FibSpec extends Specification with ScalaCheck {
  "fib" should {
    "return 0 for 0th index" in { MyModule.fib(0) mustEqual 0 }

    "return 1 for 1st index" in { MyModule.fib(1) mustEqual 1 }

    "return fib(n - 2) + fib(n - 1) for nth index" in {
      prop {
        n: Int =>
          if (n < 2) true mustEqual true
          else {
            MyModule.fib(n) mustEqual {
              MyModule.fib(n - 2) + MyModule.fib(n - 1)
            }
          }
      }
    }
  }
}


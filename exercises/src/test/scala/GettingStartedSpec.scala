import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.gettingstarted._

class MyModuleSpec extends Specification with ScalaCheck {
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

class PolymorphicFunctionsSpec extends Specification {
  "isSorted" should {
    "consider an empty Array to be sorted" in {
      true mustEqual {
        PolymorphicFunctions.isSorted(
          Array(),
          (x: Int, y: Int) => x > y
        )
      }
    }

    "consider a single-elem Array to be sorted" in {
      true mustEqual {
        PolymorphicFunctions.isSorted(
          Array(1),
          (x: Int, y: Int) => x > y
        )
      }
    }

    "consider a multi-elem sorted Array to be sorted" in {
      val sortedArr = Array(1, 2, 3, 4)

      true mustEqual {
        PolymorphicFunctions.isSorted(
          sortedArr,
          (x: Int, y: Int) => x > y
        )
      }
    }

    "consider a multi-elem unsorted Array to not be sorted" in {
      val unsortedArr = Array(1, 2, 4, 3)

      false mustEqual {
        PolymorphicFunctions.isSorted(
          unsortedArr,
          (x: Int, y: Int) => x > y
        )
      }
    }

    "consider a multi-elem Array with dup elems to be sorted if comparator defined as such" in {
      val unsortedArr = Array(1, 2, 4, 4)

      true mustEqual {
        PolymorphicFunctions.isSorted(
          unsortedArr,
          (x: Int, y: Int) => x >= y
        )
      }
    }
  }

  "curry" >> {
    "curries a function of (a, b) -> c into a -> b -> c" >> {
      val f = Math.pow _
      val curriedF = PolymorphicFunctions.curry(f)

      curriedF(3)(2) mustEqual 9
    }
  }

  "uncurry" >> {
    "uncurries a function of a -> b -> c into (a, b) -> c" >> {
      val f = PolymorphicFunctions.curry(Math.pow)
      val uncurriedF = PolymorphicFunctions.uncurry(f)

      uncurriedF(3, 2) mustEqual 9
    }
  }

  "compose" >> {
    "builds function of a -> c that sequentially carries out functions of a -> b and b -> c" >> {
      def f(i: Int) = i + 1
      def g(i: Int) = i - 1
      val x = 1

      (PolymorphicFunctions.compose(f, g))(x) mustEqual x
    }
  }
}


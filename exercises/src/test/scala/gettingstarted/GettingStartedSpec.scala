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

  def compInt(x: Int, y: Int): Boolean = x <= y
  def compChr(x: Char, y: Char): Boolean = x <= y

  "isSorted" should {
    "consider an empty Array to be sorted" in {
      true mustEqual {
        PolymorphicFunctions.isSorted(Array(),compInt)
      }
    }

    "consider a single-elem Array to be sorted" in {
      true mustEqual {
        PolymorphicFunctions.isSorted(Array('a'),compChr)
      }
    }

    "consider a multi-elem sorted Array to be sorted" in {
      val sortedArr = Array(1, 2, 3, 4)

      true mustEqual {
      	PolymorphicFunctions.isSorted(sortedArr,compInt)
     }  
      val sortedArrChar = Array('a', 'b', 'c')

      true mustEqual {
        PolymorphicFunctions.isSorted(sortedArrChar,compChr)
      }

    }

    "consider a multi-elem unsorted Array to not be sorted" in {
      val unsortedArr = Array(1, 2, 4, 3)

      false mustEqual {
      	PolymorphicFunctions.isSorted(unsortedArr,compInt)
      }

      val unSortedArrChar = Array('z', 'a')

      false mustEqual {
        PolymorphicFunctions.isSorted(unSortedArrChar,compChr)
      }
    }

    "consider a multi-elem sorted Array with dup elems to be sorted" in {
      val sortedArr = Array(1, 2, 3, 3, 3)

      true mustEqual {
      	PolymorphicFunctions.isSorted(sortedArr,compInt)
      }

      val sortedArrChar = Array('a', 'b', 'c', 'c', 'c')

      true mustEqual {
        PolymorphicFunctions.isSorted(sortedArrChar,compChr)
      }
    }
  }

  "curry" >> {
    "curries a function of (a, b) -> c into a -> b -> c" >> {
      val f = Math.max _
      val curriedF = PolymorphicFunctions.curry(f)

      curriedF(1)(2) mustEqual 2
    }
  }

  "uncurry" >> {
    "uncurries a function of a -> b -> c into (a, b) -> c" >> {
      val f = PolymorphicFunctions.curry(Math.max)
      val uncurriedF = PolymorphicFunctions.uncurry(f)

      uncurriedF(1, 2) mustEqual 2
    }
  }

  "compose" >> {
    "builds function of a -> c that sequentially carries out functions of a -> b and b -> c" >> {
      def f(i: Double) = i * 10
      def g(i: Double) = i / 10
      val x = 1 

      (PolymorphicFunctions.compose(f, g))(x) mustEqual x
    }
  }
}
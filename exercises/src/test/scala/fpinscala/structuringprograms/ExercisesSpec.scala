package fpinscala.structuringprograms

import org.specs2.mutable._

import Exercises._ // implementations

object ExercicesSpec extends Specification with SpecFunctions {
  "Exercices specification" title

  "Box(4, 5)" should { // ex 1
    val a = Box(4, 5)

    "be taller than Box(3, 5)" in {
      taller(a, Box(3, 5)) aka "taller box" mustEqual a
    }

    "be less tall than Box(5, 5)" in {
      val b = Box(5, 5)
      taller(a, b) aka "taller box" mustEqual b
    }

    "be wider than Box(4, 4)" in {
      wider(a, Box(4, 4)) aka "wider box" mustEqual a
    }

    "be less wide than Box(4, 6)" in {
      val b = Box(4, 6)
      wider(a, b) aka "wider box" mustEqual b
    }
  }

  "Higher-order function `absolute`" should { // ex 2 and 3
    "return 1" in {
      absNeg(1) aka "abs(0 - 1)" mustEqual 1
    }

    "return 4" in {
      absAdd2(2) aka "abs(2 + 2)" mustEqual 4
    }

    "return 4" in {
      absNeglen("test") aka """abs(0 - "test".length)""" mustEqual 4
    }
  }

  "Predicate from divisibleBy(2)" should { // ex 4
    val divBy2: Pred[Int] = divisibleBy(2)

    "return true when applied on 4" in {
      divBy2(4) aka "4 % 2 == 0" must beTrue
    }

    "return false when applied on 5" in {
      divBy2(5) aka "5 % 2 != 0" must beFalse
    }
  }

  "Function `even`" should { // ex 5
    "return true" in {
      "for 0" >> {
        even(0) aka "0" must beTrue
      }

      "for 2" >> {
        even(0) aka "2" must beTrue
      }

      "for 4" >> {
        even(0) aka "4" must beTrue
      }
    }

    "return false" in {
      "for 1" >> {
        even(1) aka "1" must beFalse
      }

      "for 5" >> {
        even(5) aka "5" must beFalse
      }
    }
  }

  "Predicate `divisibleBy3And5`" should { // ex 6
    "return true when applied to 15" in {
      divisibleBy3And5(15) aka "divisibleBy3And5(15)" must beTrue
    }

    "return false when applied to 26" in {
      divisibleBy3And5(26) aka "divisibleBy3And5(26)" must beFalse
    }
  }

  "Predicate `divisibleBy3Or5`" should { // ex 6
    "return true" in {
      "when applied to 3" >> {
        divisibleBy3Or5(3) aka "divisibleBy3Or5(3)" must beTrue
      }

      "when applied to 5" >> {
        divisibleBy3Or5(5) aka "divisibleBy3Or5(5)" must beTrue
      }

      "when applied to 15" >> {
        divisibleBy3Or5(15) aka "divisibleBy3Or5(15)" must beTrue
      }
    }

    "return false" in {
      "when applied to 2" >> {
        divisibleBy3Or5(2) aka "divisibleBy3Or5(2)" must beFalse
      }

      "when applied to 4" >> {
        divisibleBy3Or5(4) aka "divisibleBy3Or5(4)" must beFalse
      }
    }
  }

  "Currying" should { // ex 7
    "properly define `multBy3` from (_ * _)(3)" >> {
      "and return 6 when applied to 2" in {
        multBy3(2) aka "multBy3(2)" mustEqual 6
      }

      "and return 12 when applied to 4" in {
        multBy3(4) aka "multBy3(4)" mustEqual 12
      }
    }
  }

  "Uncurrying" should { // ex 8
    "properly define `divisible(k, i)` from `divisibleBy(k)(i)`" >> {
      "and return true when applied to (2, 4)" in {
        divisible(2, 4) aka "4 % 2 == 0" must beTrue
      }

      "and return false when applied to (2, 5)" in {
        divisible(2, 5) aka "5 % 2 == 0" must beFalse
      }
    }
  }

  "Composition" should { // ex 9
    "properly work with +4 and /2" >> {
      "and return 2 when apply to 0" in {
        add4Div2(0) aka "0+4/2" mustEqual 2
      }
    }
  }

  "Higher-order function `lift3`" should { // ex 10 and 11
    "properly define `lifted` function" in {
      lifted(12) aka "generated string" mustEqual "B12C12D12"
    }
  }

  "Function `fib`" should { // ex 12
    "start with 0" in {
      fib(0) aka "fib(0)" mustEqual 0
    }

    "return 1 at first position" in {
      fib(1) aka "first Fibonacci number" mustEqual 1
    }

    "return 1 at second position" in {
      fib(2) aka "second Fibonacci number" mustEqual 1
    }

    "return 2 at third position" in {
      fib(3) aka "third Fibonacci number" mustEqual 2
    }

    "return 5 at fifth position" in {
      fib(5) aka "fith Fibonacci number" mustEqual 5
    }
  }

  "Function `sqrt` defined using `iterateWhile`" should { // ex 13
    "work with 16" in {
      sqrt(16) aka "sqrt(16)" mustEqual 4
    }

    "work with 9" in {
      sqrt(9) aka "sqrt(9)" mustEqual 3
    }
  }
}

trait SpecFunctions {
  // ex 2 and 3
  def neg(i: Int): Int = 0 - i
  def absNeg = absolute(neg _)
  def add2(i: Int): Int = i + 2
  def absAdd2 = absolute(add2 _)
  def neglen(s: String): Int = 0 - s.length
  def absNeglen = absolute(neglen _)

  // ex 7
  def multBy3 = curry[Int, Int, Int](_ * _)(3)

  // ex 8
  def divisible = uncurry(divisibleBy _)

  // ex 9
  def add4Div2 = compose[Int, Int, Int](_ / 2, _ + 4)

  // ex 10 and 11
  def lifted: Int ⇒ String =
    lift3[Int, String, String, String, String]((b, c, d) ⇒ b + c + d)(
      "B" + _, "C" + _, "D" + _)

}

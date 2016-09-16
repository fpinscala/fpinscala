package fpinscala.gettingstarted

import org.scalatest.{FunSpec, Matchers}
import PolymorphicFunctions.isSorted

class PolymorphicFunctionsSpec extends FunSpec with Matchers {

  describe("isSorted()") {
    it("should return true for an empty Array") {
      val comparisonFn = (a: Int, b: Int) => a < b
      val given = Array()

      isSorted(given, comparisonFn) shouldBe true
    }

    it("should return true for one element Array") {
      val comparisonFn = (a: Int, b: Int) => a < b
      val given = Array(1337)

      isSorted(given, comparisonFn) shouldBe true
    }

    it("should return true if elements are sorted for two element array") {
      val comparisonFn = (a: Int, b: Int) => a < b
      val given = Array(1337, 1338)

      isSorted(given, comparisonFn) shouldBe true
    }

    it("should return false if elements are not sorted for two element array") {
      val comparisonFn = (a: Int, b: Int) => a < b
      val given = Array(1338, 1337)

      isSorted(given, comparisonFn) shouldBe false
    }

    it("should return true if elements are sorted for more than two element array") {
      val comparisonFn = (a: Int, b: Int) => a < b
      val given = Array(1, 1337, 1338)

      isSorted(given, comparisonFn) shouldBe true
    }

    it("should return false if elements are not sorted for more than two element array") {
      val comparisonFn = (a: Int, b: Int) => a < b
      val given = Array(1337, 1, 1338)

      isSorted(given, comparisonFn) shouldBe false
    }
  }
}

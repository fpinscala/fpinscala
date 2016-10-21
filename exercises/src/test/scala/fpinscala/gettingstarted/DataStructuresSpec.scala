package fpinscala.gettingstarted

import org.scalatest.{FunSpec, Matchers}
import fpinscala.datastructures.List
import fpinscala.datastructures.Nil
import fpinscala.datastructures.List.{setHead, tail, drop, dropWhile, init, foldLeft}

class DataStructuresSpec extends FunSpec with Matchers {
  describe("Exercise 3.2: tail()") {
    it("should return List(2,3) for given List(1,2,3)") {
      tail(List(1, 2, 3)) shouldBe List(2, 3)
    }

    it("should return Nil for given Nil") {
      tail(Nil) shouldBe Nil
    }
  }

  describe("Exercise 3.3: setHead()") {
    it("should return the list with given element if given list is Nil") {
      setHead(Nil, 1337) shouldBe List(1337)
    }

    it("should return List(1337) for given List(42)") {
      setHead(List(42), 1337) shouldBe List(1337)
    }

    it("should return List(42, 1337) for given List(1, 1337) and element 42") {
      setHead(List(1, 1337), 42) shouldBe List(42, 1337)
    }
  }

  describe("Exercise 3.4: drop()") {
    it("should return given list if n = 0") {
      drop(List(1,2,3), 0) shouldBe List(1,2,3)
    }

    it("should remove first element") {
      drop(List(1,2,3), 1) shouldBe List(2,3)
    }

    it("should remove first two elements") {
      drop(List(1,2,3), 2) shouldBe List(3)
    }

    it("should return empty list if n equals to the size of the list") {
      drop(List(1,2,3), 3) shouldBe List()
    }

    it("should return empty list if n is greater than the size of the list") {
      drop(List(1,2,3), 4) shouldBe List()
    }

    it("should return empty list given list is empty") {
      drop(List(), 2) shouldBe List()
    }
  }

  describe("Exercise 3.5: dropWhile()") {
    it("should return given list if first element doesn't match the predicate") {
      dropWhile(List(1,2,3), (x: Int) => x < 1) shouldBe List(1,2,3)
    }

    it("should drop one element if first element matches the predicate") {
      dropWhile(List(1,2,3), (x: Int) => x <= 1) shouldBe List(2,3)
    }

    it("should drop two elements if first two elements match the predicate") {
      dropWhile(List(1,2,3), (x: Int) => x <= 2) shouldBe List(3)
    }

    it("should return empty list if all elements match the predicate") {
      dropWhile(List(1,2,3), (x: Int) => x < 4) shouldBe List()
    }
  }

  describe("Exercise 3.6: init()") {
    it("should return List(1,2,3) for given List(1,2,3,4)") {
      init(List(1,2,3,4)  ) shouldBe List(1,2,3)
    }

    it("should return List() for given List(1)") {
      init(List(1)) shouldBe List()
    }

    it("should throw ig given list is empty") {
      assertThrows[IllegalArgumentException] {
        init(List())
      }
    }
  }

  describe("Exercise 3.9: length()") {
    it("should return 0 for an empty list") {
      List.length(List()) shouldBe 0
    }

    it("should return the list for non-empty list") {
      List.length(List('P','r','i','n','c','e','s','s')) shouldBe 8
    }
  }

  describe("Exercise 3.10: foldLeft()") {
    it("should return 0 for List(0)") {
      foldLeft(List(0), 0)((x, y) => x + y) shouldBe 0
    }

    it("should sum the list") {
      foldLeft(List(1, 2, 3, 4), 0)((x, y) => x + y) shouldBe 10
    }
  }

  describe("Exercise 3.11: sumUsingFoldLeft()") {
    it("should return 10 for List(1,2,3,4)") {
      List.sumUsingFoldLeft(List(1,2,3,4)) shouldBe 10
    }
  }

  describe("Exercise 3.11: productUsingFoldLeft()") {
    it("should return 24 for List(1,2,3,4)") {
      List.productUsingFoldLeft(List(1, 2, 3, 4)) shouldBe 24
    }
  }

  describe("Exercise 3.11: lengthUsingFoldLeft()") {
    it("should return 0 for an empty list") {
      List.lengthUsingFoldLeft(List()) shouldBe 0
    }

    it("should return 4 for List('a','b','c','d')") {
      List.lengthUsingFoldLeft(List('a', 'b', 'c', 'd')) shouldBe 4
    }
  }

  describe("Exercise 3.12: reverse()") {
    it("should return List() for given List()") {
      List.reverse(List()) shouldBe List()
    }

    it("should return List(3,2,1) for given List(1,2,3)") {
      List.reverse(List(1,2,3)) shouldBe List(3,2,1)
    }
  }
}

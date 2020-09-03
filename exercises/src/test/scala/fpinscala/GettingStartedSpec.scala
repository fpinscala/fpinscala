package fpinscala

import fpinscala.gettingstarted._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class GameStateSpec extends AnyFlatSpec with Matchers {

  it should "return the fibonacci sequence" in {
    val mod = MyModule
    mod.fib(0) shouldBe 0
    mod.fib(1) shouldBe 1
    mod.fib(2) shouldBe 1
    mod.fib(3) shouldBe 2
    mod.fib(4) shouldBe 3
    mod.fib(5) shouldBe 5
  }

  it should "return if a list is sorted" in {
    val mod = PolymorphicFunctions
    mod.isSorted(Array(1, 2, 3), (a: Int, b: Int) => a > b) shouldBe true
    mod.isSorted(Array(1, 3, 2), (a: Int, b: Int) => a > b) shouldBe false
  }
}
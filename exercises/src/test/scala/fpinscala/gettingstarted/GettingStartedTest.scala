package fpinscala.gettingstarted

import org.scalatest.{FunSuite, Matchers}


/**
  * You can write tests for your implementations using scalatest.
  *
  * The scalatest library provides lots of ways to test your code, the
  * documentation has more information
  * http://www.scalatest.org/user_guide/using_matchers
  *
  * You can run the tests from the `sbt` console with the `test` command.
  */
class GettingStartedTest extends FunSuite with Matchers {
  test("factorial of 0 is 1") {
    MyModule.factorial(0) shouldEqual 1
  }

  test("factorial of 1 is 1") {
    MyModule.factorial(1) shouldEqual 1
  }

  test("factorial of 10 is 3628800") {
    MyModule.factorial(10) shouldEqual 3628800
  }

  test("some property of fib is true") {
    // test some property of MyModule.fib(<int>)
  }

  // etc
}

package fpinscala.gettingstarted

import scala.annotation.tailrec

/**
 * Code for computing absolute value and factorial; intro to tail recursive functions
 */
object FormatAbsAndFactorial {

  def abs(n: Int): Int =
    if (n < 0) -n else n

  def factorial(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int): Int = {
      n match {
        case 0 ⇒ acc
        case _ ⇒ loop(n - 1, n * acc)
      }
    }

    loop(n, 1)
  }

  def formatResult(name: String, n: Int, f: Int ⇒ Int): String =
    s"The $name of $n is ${f(n)}"

  def main(args: Array[String]) {
    println()
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println()
  }
}

/**
 * Code for computing the nth Fibonacci number; more tail recursion
 */
object TestFibonacci {

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, p: Int, c: Int): Int = {
      n match {
        case 0 ⇒ p
        case _ ⇒ loop(n - 1, c, p + c)
      }
    }

    loop(n, 0, 1)
  }

  def main(args: Array[String]) {
    println()
    println("Expected: 0, 1, 1, 2, 3, 5, 8")
    println("Actual:   %d, %d, %d, %d, %d, %d, %d".format(fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6)))
    println()
  }
}

/**
 * Code for anonymous functions
 */
object AnonymousFunctions {
  import FormatAbsAndFactorial._

  def main(args: Array[String]) {
    println()
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("increment (v1)", 7, (x: Int) ⇒ x + 1))
    println(formatResult("increment (v2)", 7, (x) ⇒ x + 1))
    println(formatResult("increment (v3)", 7, x ⇒ x + 1))
    println(formatResult("increment (v4)", 7, _ + 1))
    println()
  }
}

/**
 * Code for monomorphic functions
 */
object MonomorphicFunctions {

  def findFirst(ss: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }

    loop(0)
  }

  def binarySearch(ds: Array[Double], key: Double): Int = {
    @tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1 // Base case
      else { //
        val mid2 = (low + high) / 2 // Compute the new middle index
        val d = ds(mid2) // Fetch the element at the middle index
        if (d == key) mid2 // You found the key
        else if (d > key) go(low, mid2, mid2 - 1) // Key is greater; search first half
        else go(mid2 + 1, mid2, high) // Key is smaller; search second half
      }
    }

    go(0, 0, ds.length - 1)
  }
}

object PolymorphicFunctions {

  def findFirst[T](ss: Array[T], p: T ⇒ Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (p(ss(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  def binarySearch[T](ts: Array[T], key: T, gt: (T, T) ⇒ Boolean): Int = {
    @tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val elem = ts(mid2)

        if (!gt(elem, key) && !gt(key, elem)) mid2
        else if (gt(elem, key)) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }

    go(0, 0, ts.length - 1)
  }

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted (ascending order)
  def isSorted[A](as: Array[A], gt: (A, A) ⇒ Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true // once we go past the end of the array, we know it's sorted
      else if (gt(as(n), as(n + 1))) false // if n is greater than n + 1, we know it's not sorted
      else loop(n + 1) // keep going otherwise
    }

    loop(0)
  }

  // Exercise 3: Implement `curry`.
  //
  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A, B, C](f: (A, B) ⇒ C): A ⇒ B ⇒ C =
    a ⇒ b ⇒ f(a, b)

  // Exercise 4: Implement `uncurry`
  def uncurry[A, B, C](f: A ⇒ B ⇒ C): (A, B) ⇒ C =
    (a, b) ⇒ f(a)(b)

  // Exercise 5: Implement `compose`
  def compose[A, B, C](f: B ⇒ C, g: A ⇒ B): A ⇒ C =
    a ⇒ f(g(a))

  def main(args: Array[String]) {
    val items = Array(1, 2, 3, 0)
    val gt = (a: Int, b: Int) ⇒ a > b
    val result = if (isSorted(items, gt)) "is" else "is not"

    println(s"The array from ${items.toList.toString()} $result sorted!")
  }
}

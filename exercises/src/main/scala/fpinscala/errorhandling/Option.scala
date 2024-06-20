package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

// Exercise 4.1: Implement all of the functions on `Option`
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = ???

  def getOrElse[B>:A](default: => B): B = ???

  def flatMap[B](f: A => Option[B]): Option[B] = ???

  def orElse[B>:A](ob: => Option[B]): Option[B] = ???

  def filter(f: A => Boolean): Option[A] = ???
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2: Implement the `variance` function in terms of `flatMap`
  def variance(xs: Seq[Double]): Option[Double] = ???

  // Exercise 4.3: Write a generic function `map2` that combines two `Option` values using a binary function. If either
  // `Option` value is `None`, then the return value is too
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  // Exercise 4.4: Write a function `sequence` that combines a list of `Option`s into one `Option` containing a list of
  // all the `Some` values in the original list. If the original list contains `None` even once, the result of the
  // function should be `None`; otherwise the result should be `Some` with a list of all the values
  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  // Exercise 4.5: Implement this function
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}
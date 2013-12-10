package fpinscala.errorhandling


import scala.{Option => _, Either => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = ???
  
  def getOrElse[B>:A](default: => B): B = ???
  
  def flatMap[B](f: A => Option[B]): Option[B] = ???
  
  /*
  Of course, we can also implement `flatMap` with explicit pattern matching.
  */
  def flatMap_1[B](f: A => Option[B]): Option[B] = ???
  
  def orElse[B>:A](ob: => Option[B]): Option[B] = ???
  
  /*
  Again, we can implement this with explicit pattern matching. 
  */
  def orElse_1[B>:A](ob: => Option[B]): Option[B] = ???
  
  def filter(f: A => Boolean): Option[A] = ???
  /*
  This can also be defined in terms of `flatMap`.
  */
  def filter_1(f: A => Boolean): Option[A] = ???
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // The `val x: Int = ...` declares `x` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block, as we've seen before. `case e: Exception` is a pattern matching any `Throwable` that is an exception, and which binds this value to the identifier `e`.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A `throw` Exception can be given any type, here we are annotating it with the type `Int`.
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = sys.error("todo")

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = sys.error("todo")

  def sequence[A](a: List[Option[A]]): Option[List[A]] = sys.error("todo")

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}

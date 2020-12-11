package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = if (this == None) ob else this

  def filter(f: A => Boolean): Option[A] = flatMap(v => if (f(v)) this else None)
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
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(av => b.map(bv => f(av, bv)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldLeft[Option[List[A]]](Some(Nil))((acc, x) => x.flatMap(v => acc.map(v :: _)))

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldLeft[Option[List[B]]](Some(Nil))((acc, x) => f(x).flatMap(v => acc.map(v :: _)))


  def main(args: Array[String]): Unit = {
    println(Some(1).map(_ * 2))
    println(None.map((x: Int) => x * 2))
    println()

    println(Some(1).getOrElse(5))
    println(None.getOrElse(5))
    println()

    println(Some(1).flatMap(_ => Some(666)))
    println(None.flatMap(_ => Some(666)))
    println()

    println(Some(1).orElse(Some(666)))
    println(None.orElse(Some(666)))
    println()

    println(Some(1).filter(_ < 2))
    println(None.filter((x: Int) => x < 2))
    println()

    println(variance(1.0 :: 2.0 :: 3.0 :: 4.0 :: 5.0 :: Nil))
    println(variance(Nil))
  }
}
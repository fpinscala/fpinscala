package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) =>
      Some(f(value))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(value) => f(value)
    case None => None
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(value) if f(value) => this
    case _ => None
  }
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
  def variance(xs: Seq[Double]): Option[Double] = {
    def internalFunc(m: Double)(x: Double): Double = math.pow(x - m, 2)
    mean(xs).flatMap(m => mean(xs.map(internalFunc(m))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(one), Some(two)) => Some(f(one, two))
    case _ => None
  }

  def map22[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(someA => b.map(someB => f(someA, someB)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    val ret = a match {
      case Nil => Some(List())
      case h :: t => h flatMap (hh => sequence(t) map { tt =>
        println(s"$hh ++ $tt")
        hh :: tt
      })
    }
    println(ret)
    ret
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t =>
        f(h) match {
          case Some(hh) =>
            traverse(t)(f).map(hh :: _)
          case None => None
        }
    }
  }

  def main(args: Array[String]): Unit = {
    println(traverse(List(1,2,3,4,5)){ digit =>
      if (digit < 6) Some(digit) else None
    })
  }
}
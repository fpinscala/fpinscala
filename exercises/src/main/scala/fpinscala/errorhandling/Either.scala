package fpinscala.errorhandling

// hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Either => _, Left => _, Option => _, Right => _}


sealed trait Either[+E,+A] {

 /**
   * Exercise 4.6 - Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
   */

 def map[B](f: A => B): Either[E, B] = this match {
   case Left(e)  => Left(e)
   case Right(a) => Right(f(a))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Left(e)  => Left(e)
   case Right(a) => f(a)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_)  => b
   case Right(a) => Right(a)
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   flatMap (a => b map (bb => f(a, bb)))

 /**
   * Alternative implementation that uses a for-comprehension.
   */
 def map3[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   for { aa <- this ; bb <- b } yield f(aa, bb)

}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  /**
    * Exercise 4.7 - Implement sequence and traverse for Either. These should return the first error that’s encountered,
    * if there is one.
    */

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(Nil): Either[E, List[B]])((a, acc) => f(a).map2(acc)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  /**
    * Exercise 4.8 - In this implementation, map2 is only able to report one error, even if both the name and the age
    * are invalid. What would you need to change in order to report both errors? Would you change map2 or the signature
    * of mkPerson? Or could you create a new data type that captures this requirement better than Either does, with some
    * additional structure? How would orElse, traverse, and sequence behave differently for that data type?
    */

  /*
   * There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors, a simple
   * approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:
   *
   * trait Partial[+A,+B]
   * case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
   * case class Success[+B](get: B) extends Partial[Nothing,B]
   *
   * There is a type very similar to this called `Validation` in the Scalaz library. You can implement `map`, `map2`,
   * `sequence`, and so on for this type in such a way that errors are accumulated when possible (`flatMap` is unable to
   * accumulate errors--can you see why?). This idea can even be generalized further--we don't need to accumulate
   * failing values into a list; we can accumulate values using any user-supplied binary function.
   *
   * It's also possible to use `Either[List[E],_]` directly to accumulate errors, using different implementations of
   * helper functions like `map2` and `sequence`.
   */
}

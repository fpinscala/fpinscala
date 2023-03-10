package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

// Exercise 4.6: Implement versions that operate on the `Right` value
sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = ???

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = ???

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = ???

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  // Exercise 4.7: Implement `sequence` and `traverse` for `Either`
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = ???

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

}
package fpinscala.errorhandling

import scala.annotation.tailrec
import scala.{Either => _, Left => _, Option => _, Right => _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(_) => this.asInstanceOf[Either[E, B]]
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => this.asInstanceOf[Either[E, B]]
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)
}

case class Left[+E](e: E) extends Either[E, Nothing]
case class Right[+A](a: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    @tailrec
    def sequence(acc: Either[E, List[A]])(es: List[Either[E, A]]): Either[E, List[A]] = es match {
      case x :: xs => sequence(acc.flatMap(ees => x.map(a => a :: ees)))(xs)
      case Nil => acc
    }
    sequence(Right(List()))(es).map(_.reverse)
  }

  def sequenceViaTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(a => a)

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    @tailrec
    def traverse(acc: Either[E, List[B]])(es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
      case x :: xs => traverse(acc.flatMap(ees => f(x).map(a => a :: ees)))(xs)(f)
      case Nil => acc
    }
    traverse(Right(List()))(es)(f).map(_.reverse)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }
}
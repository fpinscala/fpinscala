package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(ar => b.map(br => f(ar, br)))

  def map22[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    ar <- this
    br <- b
  } yield f(ar, br)

}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil))((h ,acc) => f(h).map2(acc)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def main(args: Array[String]): Unit = {
    println(Right(2).map(_ * 3))
    println(Right(2).flatMap(x => Right(s"yo - $x")))
    println(Right(2).orElse(Right(s"yo")))
    println(Right(2).map2(Right(s"yo"))((i, s) => s"i: $i, s: $s"))
    println()
    println(traverse(1 :: 2 :: 3 :: 9 :: Nil)(x => if (x < 5) Right(x) else Left(s"$x should be less than 5")))
    println(sequence(Right(1) :: Left(2) :: Right(3) :: Nil))
  }

}
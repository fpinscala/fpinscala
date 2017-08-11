package fpinscala.errorhandling

import scala.{Either => _, Option => _, Some => _} // hide std library versions


sealed trait Option[+A] {

  /**
    * Exercise 4.1
    *
    * Implement all of the preceding functions on Option. As you implement each function, try to
    * think about what it means and in what situations you’d use it. We’ll explore when to use each
    * of these functions next. Here are a few hints for solving this exercise:
    *
    *   - It’s fine to use pattern matching, though you should be able to implement all the
    * functions besides map and getOrElse without resorting to pattern matching.
    *   - For map and flatMap, the type signature should be enough to determine the implementation.
    *   - getOrElse returns the result inside the Some case of the Option, or if the Option is None,
    * returns the given default value.
    *   - orElse returns the first Option if it’s defined; otherwise, it returns the second Option.
    */

  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  /** Returns the mean of a sequence of doubles. */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  /**
    * Exercise 4.2
    *
    * Implement the variance function in terms of flatMap. If the mean of a sequence is m, the
    * variance is the mean of math.pow(x - m, 2) for each element x in the sequence. See the
    * definition of variance on Wikipedia (http://mng.bz/0Qsr).
    */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs map (x => math.pow(x - m, 2))))

  /**
    * Exercise 4.3
    *
    * Write a generic function map2 that combines two Option values using a binary function. If
    * either Option value is None, then the return value is too.
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  /** Version of map2 implemented using a for-comprehension for illustration. */
  def map2ForComprehension[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  /**
    * Exercise 4.4
    *
    * Write a function sequence that combines a list of Options into one Option containing a list of
    * all the Some values in the original list. If the original list contains None even once, the
    * result of the function should be None; otherwise the result should be Some with a list of all
    * the values.
    */
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))

  /**
    * Exercise 4.5
    *
    * Implement this function. It’s straightforward to do using map and sequence, but try for a more
    * efficient implementation that only looks at the list once. In fact, implement sequence in
    * terms of traverse.
    */
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]])((a, acc) => map2(f(a), acc)(_ :: _))

  /** Version of sequence in terms of traverse for illustration. */
  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)
}

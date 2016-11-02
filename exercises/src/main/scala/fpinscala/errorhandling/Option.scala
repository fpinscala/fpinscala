package fpinscala.errorhandling

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.{Either => _, Option => _, Some => _}


sealed trait Option[+A] {

  /**
    * Exercise 4.1 - Implement all of the preceding functions on Option. As you implement each function, try to think
    * about what it means and in what situations you’d use it.  We’ll explore when to use each of these functions next.
    * Here are a few hints for solving this exercise:
    *
    *   - It’s fine to use pattern matching, though you should be able to implement all the functions besides map and
    *     getOrElse without resorting to pattern matching.
    *   - For map and flatMap, the type signature should be enough to determine the implementation.
    *   - getOrElse returns the result inside the Option, or if the Option is None, returns the default value.
    *   - orElse returns the first Option if it’s defined; otherwise, it returns the second Option.
    */

  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map (f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }

    /*
     * A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that
     * matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
     */

    catch { case e: Exception => 43 }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      x + ((throw new Exception("fail!")): Int)
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * Exercise 4.2 - Implement the variance function in terms of flatMap.
    */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs map (x => math.pow(x - m, 2))))

  /**
    * Exercise 4.3 - Write a generic function map2 that combines two Option values using a binary func- tion. If either
    * Option value is None, then the return value is too.
    */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  /**
    * Exercise 4.4 - Write a function sequence that combines a list of Options into one Option containing a list of all
    * the Some values in the original list. If the original list contains None even once, the result of the function
    * should be None; otherwise the result should be Some with a list of all the values.
    *
    * Note that this implementation is not stack-safe. A stack-safe version can be provided using `reverse` and
    * `foldLeft` without changing anything else.
    */
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))

  /**
    * Exercise 4.5 - Implement this function. It’s straightforward to do using map and sequence, but try for a more
    * efficient implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
    */
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]])((a, acc) => map2(f(a), acc)(_ :: _))

  def sequence2[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)
}

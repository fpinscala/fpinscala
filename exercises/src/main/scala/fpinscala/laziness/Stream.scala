package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 5.1
  def toListRecursive: List[A] = this match {
      case Cons(h,t) => h() :: t().toListRecursive
      case _ => List()
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(str: Stream[A], accum: List[A]) : List[A] = str match {
        case Cons(h, t) => go(t(), accum :+ h())
        case _ => accum
    }

    go(this, List())
  }

  def take(n: Int): Stream[A] =
    if (n > 0) this match {
      case Cons(h, t) if n == 1 => cons(h(), Stream.empty)
      case Cons(h, t) => cons(h(), t().take(n-1))
      case _ => Stream.empty
    }
    else Stream.empty


  def drop(n: Int): Stream[A] =
    if (n > 0) this match {
      case Cons(h, t) if n == 1 => t()
      case Cons(h, t) => t().drop(n-1)
      case _ => Stream.empty
    }
    else Stream.empty

  def drop2(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], n: Int): Stream[A] =
      if (n <= 0) s
      else s match {
        case Cons(h,t) => go(t(), n-1)
        case _ => Stream()
      }
    go(this, n)
  }


  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => Stream.empty
    }

  def takeWhileFR(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h,t) =>
        if (p(h)) cons(h,t)
        else Stream.empty)

   def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) =>
      if (f(h)) cons(h,t)
      else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}

object MyStream {

  def main(args: Array[String]): Unit = {
    val x = Stream(1,4,5,-1,6,4)
    val z = Stream(3,6,9)

    println("Exercise 5.1 - Guided by answers")

    println("Input stream to list (recursive): " + x.toListRecursive)
    println("Input stream to list (recursive safe): " + x.toList)

    println("\nExercise 5.2")
    val take2_expected = List(1,4)
    println("\nx.take(2) expected = %s".format(take2_expected))
    println("x.take(2) actual = %s".format(x.take(2).toListRecursive))
    println("take function status: %s".format(x.take(2).toList == take2_expected))


    val drop3_expected = List(-1,6,4)
    println("\nx.drop(3) expected = %s".format(drop3_expected))
    println("x.drop(3) actual = %s".format(x.drop(3).toListRecursive))
    println("drop function status: %s".format(x.drop(3).toList == drop3_expected))

    println("\nExercise 5.3")
    val takeWhile_expected = List(1,4,5)
    println("\nx.takeWhile((x: Int) => if (x > 0) true else false)) expected = %s".format(takeWhile_expected))
    println("x.takeWhile((x: Int) => if (x > 0) true else false)) actual = %s".format(
        x.takeWhile((x: Int) => if (x > 0) true else false).toListRecursive))
    println("takeWhile function status: %s".
        format(x.takeWhile((x: Int) => if (x > 0) true else false).toList == takeWhile_expected))

    println("\nExercise 5.4")
    val forAll_expected = false
    println("\nx.forAll((x: Int) => if (x > 0) true else false)) expected = %s".format(forAll_expected))
    println("x.forAll((x: Int) => if (x > 0) true else false)) actual = %s".format(
        x.forAll((x: Int) => if (x > 0) true else false)))
    println("forAll function status: %s".
        format(x.forAll((x: Int) => if (x > 0) true else false) == forAll_expected))

    println("\nExercise 5.5")
    val takeWhileFR_expected = List(1,4,5)
    println("\nx.takeWhileFR((x: Int) => if (x > 0) true else false)) expected = %s".format(takeWhileFR_expected))
    println("x.takeWhileFR((x: Int) => if (x > 0) true else false)) actual = %s".format(
        x.takeWhileFR((x: Int) => if (x > 0) true else false).toListRecursive))
    println("takeWhileFR function status: %s".
        format(x.takeWhileFR((x: Int) => if (x > 0) true else false).toList == takeWhileFR_expected))

    println("\nExercise 5.7")
    val map_expected = List(6,9,12)
    println("\nz.map((x: Int) => x + 3) expected = %s".format(map_expected))
    println("z.map((x: Int) => x + 3) actual = %s".format(
        z.map((x: Int) => x + 3).toListRecursive))
    println("map function status: %s".
        format(z.map((x: Int) => x + 3).toList == map_expected))






    println(x.filter((x: Int) => if (x<4) true else false).toListRecursive)
    println(x.append(z).toListRecursive)
  }

}
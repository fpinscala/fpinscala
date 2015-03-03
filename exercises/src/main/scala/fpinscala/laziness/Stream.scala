package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // The arrow `=>` in front of the argument type `B` means that the function `f` takes 
  // its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B = 
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  // Here `b` is the unevaluated recursive step that folds the tail of the stream. 
  // If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) 

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case _ if n <= 0 => Empty
    case Empty => Empty
    case Cons(head, tail) => Stream.cons(head(), tail().take(n - 1))
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case _ if n <= 0 => this
    case Empty => Empty
    case Cons(head, tail) => tail().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { (a, b) =>
    if (p(a)) Stream.cons(a, b)
    else Empty
  }

  def forAll(p: A => Boolean): Boolean = !exists(!p(_))

  def headOption: Option[A] = foldRight(Option.empty[A]) { (a, b) => Some(a) }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B]) { (a, b) =>
    Stream.cons(f(a), b)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B]) { (a, b) =>
    f(a) append b
  }

  def filter(f: A => Boolean) = foldRight(Stream.empty[A]) { (a, b) =>
    if (f(a)) Stream.cons(a, b)
    else b
  }
  
  def append[B >: A](other: => Stream[B]): Stream[B] = foldRight(other) { (a, b) =>
    Stream.cons(a, b)
  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(head, tail) => head() :: tail().toList
  }
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
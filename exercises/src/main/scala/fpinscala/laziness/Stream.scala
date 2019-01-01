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

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] =
    if (n <= 0) empty
    else this match {
      case Empty => empty
      case Cons(h, t) => Cons(h, () => t().take(n-1))
    }

  def drop(n: Int): Stream[A] =
    if (n <= 0) this
    else this match {
      case Empty => empty
      case Cons(_, t) => t().drop(n-1)
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => if (p(h())) cons[A](h(), t().takeWhile(p)) else empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else empty )

  def headOption2: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](empty)((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatmap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  def tail: Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) => t()
  }

  // map, take, takeWhile, zipWith, zipAll using unfold
  // def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]

  def map_unfold[B](f: A => B): Stream[B] =
    unfold(this){
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def take_unfold(n: Int): Stream[A] =
    unfold((this, n)){
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), m) if m > 1 => Some((h(), (t(), m-1)))
      case _ => None
    }

  def takeWhile_unfold(p: A => Boolean): Stream[A] =
    unfold(this){
      case Empty => None
      case Cons(h, t) => if (p(h())) Option(h(), t()) else None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???

  def startsWith[B](s: Stream[B]): Boolean = ???
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
  def from(n: Int): Stream[Int] = {
    lazy val inf: Stream[Int] = cons(n, inf.map(_ + 1))
    inf
  }

  def constant[A](a: A): Stream[A] = {
    lazy val inf: Stream[A] = cons(a, inf)
    inf
  }

  // 0, 1, 1, 2, 3, 5, 8...
  def fibs: Stream[Int] = {
    def gen(f: Int, s: Int): Stream[Int] =
      cons(f, cons(s, gen(f + s, f + 2*s)))
    gen(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty[A]
      case Some((h, t)) => cons[A](h, unfold(t)(f))
    }

  // fibs, from, constant, ones using unfold

  def fibs_unfold: Stream[Int] = unfold((0,1)){ case (f,s) => Option(f, (s, f+s)) }

  def from_unfold(n: Int): Stream[Int] = unfold(n)(_ => Option(n, n+1))

  def constant_unfold[A](a: A): Stream[A] = unfold(a)(_ => Option(a, a))

  lazy val ones_unfold: Stream[Int] = unfold(1)(_ => Option((1, 1)))
}

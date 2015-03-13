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
    case Cons(head, tail) if n == 1 => cons(head(), Empty)
    case Cons(head, tail) if n > 1 => cons(head(), tail().take(n - 1))
    case _ => Empty
  }
  def take_unfold(n: Int): Stream[A] = unfold(this -> n) {
    case (_, 0) | (Empty, _) => None
    case (Cons(head, _), 1) => Some((head(), Empty -> 0))
    case (Cons(head, tail), count) => Some((head(), tail() -> (count - 1)))
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case _ if n <= 0 => this
    case Empty => Empty
    case Cons(head, tail) => tail().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(head, tail) if p(head()) => cons(head(), tail().takeWhile(p))
    case _ => Empty
  }

  def takeWhile_foldRight(p: A => Boolean): Stream[A] = foldRight(empty[A]) { (a, b) =>
    if (p(a)) cons(a, b)
    else Empty
  }

  def takeWhile_unfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(head, tail) if p(head()) => Some(head() -> tail())
    case _ => None
  }

  def forAll(p: A => Boolean): Boolean = !exists(!p(_))

  def headOption: Option[A] = foldRight(Option.empty[A]) { (a, b) => Some(a) }

  // 5.7 map, filter, append, flatMap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B]) { (a, b) =>
    cons(f(a), b)
  }

  def map_unfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(head, tail) => Some(f(head()) -> tail())
    case _ => None
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B]) { (a, b) =>
    f(a) append b
  }

  def filter(f: A => Boolean) = foldRight(empty[A]) { (a, b) =>
    if (f(a)) cons(a, b)
    else b
  }

  def append[B >: A](other: => Stream[B]): Stream[B] = foldRight(other) { (a, b) =>
    cons(a, b)
  }

  def startsWith[B](that: Stream[B]): Boolean = (this, that) match {
    case (_, Empty) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1() startsWith t2()
    case _ => false
  }

  def startsWith_zipAll_foldRight[B](that: Stream[B]): Boolean = (this zipAll that).foldRight(true) { (pair, acc) =>
    pair match {
      case (None, Some(_)) => false
      case (Some(a), Some(b)) if a != b => false
      case (_, None) => true
      case _ => acc
    }
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(head, tail) => head() :: tail().toList
  }

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] = unfold(this -> that) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), t1() -> t2()))
    case _ => None
  }

  def zip[B](that: Stream[B]): Stream[(A, B)] = zipWith(that) { (a, b) => (a, b) }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this -> that) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), t1() -> t2()))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), t1() -> Empty))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), Empty -> t2()))
    case _ => None
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case stream @ Cons(_, tail) => Some(stream -> tail())
    case Empty => None
  } append Stream(Empty)

  // unfold cannot be used while incrementally computing values
  // since it generate values from head to tail, but it needs to be
  // done from tail to head, since the head require the tail of the 
  // original stream.
  def scanRight_unfold[B](z: B)(f: (A, => B) => B): Stream[B] = unfold(this) {
    case stream @ Cons(_, tail) => Some(stream.foldRight(z)(f) -> tail())
    case Empty => None
  } append Stream(z)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight(z -> Stream(z)) { (a, pair) =>
    lazy val (acc, tail) = pair
    val head = f(a, acc)
    head -> cons(head, tail)
  }._2

  def tails_scanRight: Stream[Stream[A]] = scanRight(empty[A])(cons(_, _))
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

  def constants[A](a: A): Stream[A] = cons(a, constants(a))

  def constants_unfold[A](a: A): Stream[A] = unfold(a) { _ => Some((a, a)) }

  val ones: Stream[Int] = constants(1)

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def from_unfold(n: Int): Stream[Int] = unfold(n) { next => Some(next, next + 1) }

  def fibs: Stream[Int] =
    from(0).map { n =>
      if (n == 0) 0
      else if (n == 1) 1
      else fibs.drop(n - 2).take(2).foldRight(0)(_ + _) // fibs(n - 2) + fibs(n - 1)
    }

  def fibs_2: Stream[Int] = {
    def fibs(a: Int, b: Int): Stream[Int] = cons(a, fibs(b, a + b))
    fibs(0, 1)
  }

  def fibs_unfold: Stream[Int] = Stream(0, 1) append unfold((0, 1)) { case (a, b) => Some(a + b, (b, a + b)) }
  def fibs_unfold_2: Stream[Int] = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).fold(empty[A]) { case (next, state) =>
      cons(next, unfold(state)(f))
    }
  }
}
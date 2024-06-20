package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // Exercise 5.1: Write a function to convert a `Stream` into a `List`, which will force its evaluation and let you
  // look at it in the REPL

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

  // Exercise 5.2: Write the function `take(n)` for returning the first `n` elements of a `Stream`
  def take(n: Int): Stream[A] = ???

  // Exercise 5.2: Write the function `drop(n)` for skipping the first `n` elements of a `Stream`
  def drop(n: Int): Stream[A] = ???

  // Exercise 5.3: Write the function `takeWhile` for returning all starting elements of a `Stream` that match the given
  // predicate
  def takeWhile(p: A => Boolean): Stream[A] = ???

  // Exercise 5.4: Implement `forAll`, which checks that all elements in the `Stream` match a given predicate. Your
  // implementation should terminate the traversal as soon as it encounters a nonmatching value
  def forAll(p: A => Boolean): Boolean = ???

  // Exercise 5.5: Use `foldRight` to implement `takeWhile`

  // Exercise 5.6: Use `foldRight` to implement `headOption`
  def headOption: Option[A] = ???

  // Exercise 5.7: Implement `map`, `filter`, `append` and `flatMap` using `foldRight`. Part of the exercise is writing
  // your own function signatures.

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

  // Exercise 5.8: Write a function `constant` to return an infinite `Stream` of a given value

  // Exercise 5.9: Write a function that generates an infinite stream of integers, starting from `n`, then `n + 1`,
  // `n + 2`, and so on
  def from(n: Int): Stream[Int] = ???

  // Exercise 5.10: Write a function `fibs` that generates the infinite stream of Fibonacci numbers

  // Exercise 5.11: Write a more general stream-building function called `unfold`. It takes an initial state, and a
  // function for producing both the next state and the next value in the generated stream
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  // Exercise 5.12: Write fibs, from, constant, and ones in terms of unfold

  // Exercise 5.13: Use `unfold` to implement `map`, `take`, `takeWhile`, `zipWith` and `zipAll`

  // Exercise 5.14: Implement `startsWith` using functions already written

  // Exercise 5.15: Implement `tails` using `unfold`

  // Exercise 5.16: Generalise `tails` to the function `scanRight`
}
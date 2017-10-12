package fpinscala.laziness

import fpinscala.laziness.Stream._


trait Stream[+A] {

  /** Lazy version of `foldRight` for lists. */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      // If `f` doesn't evaluate its second argument, the recursion never occurs
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  /** Short-circuiting version of `exists` using `foldRight`. */
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /**
    * Exercise 5.1
    *
    * Write a function to convert a `Stream` to a `List`, which will force its evaluation and let
    * you look at it in the REPL. You can convert to the regular `List` type in the standard
    * library. You can place this and other functions that operate on a `Stream` inside the `Stream`
    * trait.
    */
  def toList: List[A] =
    foldRight(Nil: List[A])(_ :: _)

  /** Tail-recursive version for illustration. */
  def toListTailRecursive: List[A] = {
    val buffer = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def loop(s: Stream[A]): List[A] = s match {
      case Empty      => buffer.toList
      case Cons(h, t) => buffer += h(); loop(t())
    }

    loop(this)
  }

  /**
    * Exercise 5.2
    *
    * Write the function `take(n)` for returning the first `n` elements of a `Stream`, and `drop(n)`
    * for skipping the first `n` elements of a `Stream`.
    */

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  /**
    * Exercise 5.3
    *
    * Write the function `takeWhile` for returning all starting elements of a `Stream` that match
    * the given predicate.
    */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _                    => empty
  }

  /**
    * Exercise 5.4
    *
    * Implement `forAll`, which checks that all elements in the `Stream` match a given predicate.
    * Your implementation should terminate the traversal as soon as it encounters a nonmatching
    * value.
    */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, q) => p(h) && q)

  /**
    * Exercise 5.5
    *
    * Use `foldRight` to implement `takeWhile`.
    */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, s) => if (p(a)) cons(a, s) else s)

  /**
    * Exercise 5.6
    *
    * Hard: Implement `headOption` using `foldRight`.
    */
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  /**
    * Exercise 5.7
    *
    * Implement `map`, `filter`, `append`, and `flatMap` using `foldRight`. The `append` method
    * should be non-strict in its argument.
    */

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, acc) => if (p(a)) cons(a, acc) else acc)

  def append[AA >: A] (s: Stream[AA]): Stream[AA] =
    foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((a, acc) => f(a) append acc)

  /**
    * Exercise 5.13 - Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll. The zipAll
    * function should continue the traversal as long as either stream has more elements—it uses Option to indicate
    * whether each stream has been exhausted.
    */

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None // this is needed to terminate evaluation
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((n, this)) {
      case (n, Cons(h, t)) if n > 1 => Some((h(), (n - 1, t())))
      case (1, Cons(h, _))          => Some((h(), (0, empty)))
      case (_, _)                   => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _                    => None
    }

  def zipWithViaUnfold[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h, t), Empty)          => Some(((Some(h()), None), (t(), empty)))
      case (Empty, Cons(h, t))          => Some(((None, Some(h())), (empty, t())))
      case (Empty, Empty)               => None
    }

  /**
    * Exercise 5.14 - Hard: Implement startsWith using functions you’ve written. It should check if one Stream is a
    * prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
    *
    * Note that this implementation uses `zipAll` rather than `zipWith` to generate the pairs of elements to be tested
    * for equality. This is because `zipAll` is better suited to handle empty streams. If the initial stream is empty,
    * we want this function to return `false` if the second stream is non-empty.
    */
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s) takeWhileViaUnfold { case (_, b) => b.nonEmpty } forAll { case (a, b) => a == b }

  /**
    * Exercise 5.15 - Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes of the
    * input sequence, starting with the original Stream. For example, given Stream(1,2,3), it would return
    * Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
    *
    * Note the use of `drop` to pass the tail of the current stream to the next iteration of `unfold`. This can be done
    * in other ways, but using `drop`, which we've already defined, is more elegant.
    *
    * Note also that we append the empty stream to the result of the `unfold`.
    */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s     => Some((s, s drop 1))
    } append Stream(empty)

  /**
    * Exercise 5.16 - Hard: Generalize tails to the function scanRight, which is like a foldRight that returns a stream
    * of the intermediate results. For example:
    *
    * scala> Stream(1,2,3).scanRight(0)(_ + _).toList
    * res0: List[Int] = List(6,5,3,0)
    *
    * Note that this cannot be implemented in terms of `unfold` since `unfold` generates elements of the stream from
    * left to right. It can be implemented in terms of `foldRight` though.
    *
    * A sample derivation for the example above looks like:
    *
    * Stream(1, 2, 3).scanRight(0)(_ + _)
    *
    * define our initial value:
    *
    * val init: (B, Stream[B]) = (0, Stream(0))
    *
    * define our combiner function:
    *
    * val combiner: (A, => (B, Stream[B])) => (B, Stream[B]) = (a, t) => {
    * lazy val p = t
    * val b = f(a, p._1)
    *
    * (b, cons(b, p._2))
    * }
    *
    * becomes:
    * Stream(1, 2, 3).foldRight(init)(combiner)
    *
    * becomes:
    * Cons(1, Cons(2, Cons(3, Empty))).foldRight(init)(combiner)
    *
    * becomes:
    * combiner(1, combiner(2, combiner(3, init)))
    *
    * becomes:
    * val inner1 = combiner(3, init)   ≈ (3 + 0, Stream(3 + 0, 0))
    * val inner2 = combiner(2, inner1) ≈ (2 + 3 + 0, Stream(2 + 3 + 0, 3 + 0, 0))
    * val inner3 = combiner(1, inner2) ≈ (1 + 2 + 3 + 0, Stream(1 + 2 + 3 + 0, 2 + 3 + 0, 3 + 0, 0))
    *
    * becomes:
    * (6, Stream(6, 5, 3, 0))
    *
    * take the second element of our tuple:
    * Stream(6, 5, 3, 0)
    */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, acc) => {
      lazy val acc1 = acc
      val b = f(a, acc1._1)

      (b, cons(b, acc1._2))
    }) match {
      case (_, s) => s
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  /** Smart constructor for nonempty streams */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  /** Smart constructor for empty streams */
  def empty[A]: Stream[A] = Empty

  /** Convenient variable argument method for constructing a stream */
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /** A stream of 1s; note the recursive definition */
  val ones: Stream[Int] = Stream.cons(1, ones)

  /**
    * Exercise 5.8
    *
    * Generalize `ones` slightly to the function `constant`, which returns an infinite `Stream` of a
    * given value.
    */
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  /** A more efficient version of `constant` which makes the tail lazy. */
  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)

    tail
  }

  /**
    * Exercise 5.9
    *
    * Write a function that generates an infinite stream of integers, starting from `n`, then `n +
    * 1`, `n + 2`, and so on.
    */
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  /**
    * Exercise 5.10
    *
    * Write a function `fibs` that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2,
    * 3, 5, 8, and so on.
    */
  def fibs: Stream[Int] = {
    def loop(previous: Int, current: Int): Stream[Int] =
      cons(previous, loop(current, previous + current))

    loop(0, 1)
  }

  /**
    * Exercise 5.11
    *
    * Write a more general stream-building function called `unfold`. It takes an initial state, and
    * a function for producing both the next state and the next value in the generated stream.
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) map { case (a, s) => cons(a, unfold(s)(f)) } getOrElse empty

  /**
    * Exercise 5.12
    *
    * Write `fibs`, `from`, `constant`, and `ones` in terms of `unfold`.
    */
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (previous, current) => Some((previous, (current, previous + current))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(i => Some((i, i +  1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))
}

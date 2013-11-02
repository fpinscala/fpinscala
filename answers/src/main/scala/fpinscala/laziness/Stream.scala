package fpinscala.laziness

import Stream._

sealed abstract class Stream[+A] { // The abstract base class for streams. It will have only two sub-classes, one for the empty stream and another for the nonepty stream.
  def uncons: Option[Cons[A]] // The abstract interface of `Stream` is simply this one abstract method, `uncons`. Calling `uncons` will give us either `None` if it's empty or `Some` if it's nonempty. In the nonempty case, a `Cons` consists of the head and the tail.
  def isEmpty: Boolean = uncons.isEmpty
  // The natural recursive solution
  def toListRecursive: List[A] = uncons match {
    case Some(c) => c.head :: c.tail.toList
    case _ => List()
  }
  
  /*
  The above solution will stack overflow for large streams, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the stream. Then at the end we reverse the result to get the
  correct order again.
  */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s uncons match {
      case Some(c) =>
        go(c.tail, c.head :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }
  
  /* 
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A] 
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s uncons match {
      case Some(c) =>
        buf += c.head
        go(c.tail)
      case _ => buf.toList
    }
    go(this)
  }

  
  /*
  `take` first checks if n==0. In that case we need not look at the stream at all.
  When n==1 we only need to look at the head of the stream, so that is a special case.
  */
  def take(n: Int): Stream[A] = 
    if (n > 0) uncons match {
      case Some(c) if (n == 1) => cons(c.head, Stream())
      case Some(c) => cons(c.head, c.tail.take(n-1))
      case _ => Stream()
    }
    else Stream()

  /*
  It's a common Scala style to write method calls without `.` notation, as in `c.tail takeWhile f`.  
  */
  def takeWhile(f: A => Boolean): Stream[A] = uncons match { 
    case Some(c) if f(c.head) => cons(c.head, c.tail takeWhile f)
    case _ => empty 
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    uncons match {
      case Some(c) => f(c.head, c.tail.foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case None => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  /* 
  Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element is found. 
  */
  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a,b) => f(a) && b)

  def takeWhile_1(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => 
      if (f(h)) cons(h,t)
      else      empty)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t)) 
  
  def filter[B](f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => 
      if (f(h)) cons(h, t)
      else t) 
  
  def append[B>:A](s: Stream[B]): Stream[B] = 
    foldRight(s)((h,t) => cons(h,t))
  
  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((h,t) => f(h) append t)

  /*
  `uncons` has exactly the correct signature needed for `unfold`. (Notice that for any `Stream`, `s`, `unfold(s)(_.uncons) == s`. To implement `Stream.map`, we simply transform the first element of the pair returned, using the `map` method on `Option` (discussed in chapter 4). Variation 1 has the same function written using explicit pattern matching on `Option`.
  */
  def mapViaUnfold[B](f: A => B): Stream[B] = 
    unfold(this)(_.uncons.map { c => (f(c.head), c.tail) })
  
  def mapViaUnfold_1[B](f: A => B): Stream[B] = 
    unfold(this)(_.uncons match {
      case None => None
      case Some(c) => Some((f(c.head), c.tail))
    })
  
  def takeViaUnfold(n: Int): Stream[A] = 
    unfold((this,n)) { 
      case (s,n) if n > 0 => 
        s.uncons.map { c => (c.head, (c.tail, n)) }
      case _ => None
    }
  
  /*
  Notice we are using a _pattern label_ here. In front of any pattern, `x`, we can say `labelName@x`, to introduce a variable we can reference on the right-hand side. Here we are giving `Some((h,_))` a name to allow us to simply reuse the value (rather than reconstructing it) if the guard (in this case `if f(h)`) passes. 
  */
  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = 
    unfold(this)(s => s.uncons match { 
      case s@Some(c) if f(c.head) => Some(c.head, c.tail)
      case _ => None
    })
  
  def zip[B](s2: Stream[B]): Stream[(A,B)] = 
    zipWith(s2)((_,_))
  
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = 
    unfold((this, s2)) { case (s1,s2) => 
      (s1.uncons, s2.uncons) match {
        case (Some(c1), Some(c2)) => Some((f(c1.head, c2.head), (c1.tail, c2.tail)))
        case _ => None
      }
    }
  
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = 
    zipWithAll(s2)((_,_))
  
  /* 
  There are a number of edge cases with this function. We can deal with some of these edge cases by treating each stream as an infinite series of `Option` values, which become `None` when the stream is exhausted. 
  */
  def zipWithAll[B,C](s2: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] = {
    val a = this map (Some(_)) append (constant(None)) 
    val b = s2 map (Some(_)) append (constant(None)) 
    unfold((a, b)) {
      case (s1,s2) => for {
        c1 <- s1.uncons
        c2 <- s2.uncons
      } yield (f(c1.head, c2.head), (c1.tail, c2.tail))
    }
  }

  /*
  The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
  */
  def tails: Stream[Stream[A]] = 
    unfold(this)(s => s.uncons match {
      case None => None 
      case Some(c) => Some((s, c.tail))
    }) append (Stream(empty))

  /*
  The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.
  
  The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
  */
  def scanRight[B](z: B)(f: (A,=>B) => B): Stream[B] = 
    foldRight((z, Stream(z)))((a,p) => {
      val b2 = f(a,p._1)
      (b2, cons(b2,p._2))
    }) _2
}

object Empty extends Stream[Nothing] {
  val uncons = None // A concrete implementation of `uncons` for the empty case. The empty stream is represented by `None`. Note the use of a `val` in this concrete implementation.
}

sealed abstract class Cons[+A] extends Stream[A] { // A nonempty stream consists of a head and a tail and its `uncons` implementation is simply itself wrapped in `Some`.
  def head: A // Note that head and tail are abstract. A concrete implementation is given by the `cons` method in the Stream companion object.
  def tail: Stream[A]
  val uncons = Some(this)
}

object Stream {
    def empty[A]: Stream[A] = Empty // A "smart constructor" for creating an empty stream of a particular type.
  
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] { // A "smart constructor" for creating a nonempty stream.
      lazy val head = hd // The head and tail are implemented by lazy vals.
      lazy val tail = tl
    }
  
    def apply[A](as: A*): Stream[A] = // A convenient variable-argument method for constructing a `Stream` from multiple elements.
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constant[A](a: A): Stream[A] = new Cons[A] {
    val head = a
    lazy val tail = this
  }

  def from(n: Int): Stream[Int] = new Cons[Int] {
    val head = n
    lazy val tail = from(n+1)
  }

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] = 
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  /*
  Scala provides shorter syntax when the first action of a function literal is to match on an expression.  The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p => p match { case (f0,f1) => ... }`, but we avoid having to choose a name for `p`, only to pattern match on it.
  */
  val fibsViaUnfold = cons(0, 
    unfold((0,1)) { case (f0,f1) => Some((f1,(f1,f0+f1))) })
  
  def fromViaUnfold(n: Int) = 
    unfold(n)(n => Some((n,n+1)))
  
  def constantViaUnfold[A](a: A) = 
    unfold(a)(_ => Some((a,a)))
  
  // could also of course be implemented as constant(1)
  val onesViaUnfold = unfold(1)(_ => Some((1,1)))

  /* 
  s.startsWith(s2) when corresponding elements of s and s2 are all equal, until the point that s2 is exhausted. If s is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness, we can compose these three separate logical steps - the zipping, the termination when the second stream is exhausted, and the termination if a nonmatching element is found or the first stream is exhausted.
  */
  def startsWith[A](s1: Stream[A], s: Stream[A]): Boolean = 
    s1.zipAll(s).takeWhile(!_._2.isEmpty) forAll { 
      case (Some(h),Some(h2)) if h == h2 => true 
      case _ => false
    }

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails exists (startsWith(_,s2))
}
// In this alternate version of `Stream`, the abstract method is `foldRight`
// instead of `uncons`. We give it two concrete implementations, one
// in `Empty` and one in `Cons`.
// The fact that this is possible means that `foldRight` is in some sense
// a primitive `Stream` operation in terms of which all other operations
// can be written.
// It can be a more efficient representation because we don't need to 
// construct those intermediate `Option`s during the fold.
// Practically speaking, both `uncons` and `foldRight` should be abstract in
// `Stream`, not with one implemented in terms of the other.
sealed abstract class Stream2[+A] {
  def foldRight[B](z: => B)(f: (A, => B) => B): B

  def uncons: Option[Cons2[A]] =
    foldRight(None:Option[Cons2[A]])((h, r) =>
      Some(Stream2.cons2(h, r.getOrElse(Empty2))))
}

case object Empty2 extends Stream2[Nothing] {
  // Folding the empty stream results in the base case.
  def foldRight[B](z: => B)(f: (Nothing, => B) => B) = z

}
sealed abstract class Cons2[+A] extends Stream2[A] {
  def head: A
  def tail: Stream2[A]

  // Folding a nonempty stream applies f to the head and the fold of the tail.
  def foldRight[B](z: => B)(f: (A, => B) => B) = 
    f(head, tail.foldRight(z)(f))
}

object Stream2 {
  def cons2[A](hd: => A, tl: => Stream2[A]): Cons2[A] = new Cons2[A] {
    lazy val head = hd
    lazy val tail = tl
  }
}
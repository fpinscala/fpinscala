package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // The natural recursive solution
  def toListRecursive: List[A] = this match {
    case Cons(h,t) => h() :: t().toListRecursive
    case _ => List()
  }
  
  /*
  The above solution will stack overflow for large streams, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the stream. Then at the end we reverse the result to get the
  correct order again.
  [:ben] are the line breaks above okay? I'm unclear on whether these "hints" are supposed to go in the book or not
  */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
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
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  
  /*
  `take` first checks if n==0. In that case we need not look at the stream at all.
  */
  def take(n: Int): Stream[A] = 
    if (n > 0) this match {
      case Cons(h, t) => cons(h(), t().take(n-1))
      case _ => Stream.empty // we can say Stream.empty
    }
    else Stream()            // or Stream()
  
  /* 
  Unlike `take`, `drop` is not incremental. That is, it doesn't generate the
  answer lazily. It must traverse the first `n` elements of the stream eagerly.
  */
  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], n: Int): Stream[A] =
      if (n <= 0) s
      else s match {
        case Cons(h,t) => go(t(), n-1) 
        case _ => Stream()
      }
    go(this, n)
  }

  /*
  It's a common Scala style to write method calls without `.` notation, as in `t() takeWhile f`.  
  */
  def takeWhile(f: A => Boolean): Stream[A] = this match { 
    case Cons(h,t) =>
      val head = h()
      if (f(head)) cons(head, t() takeWhile f) else empty
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
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

  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t)) 
  
  def filter[B](f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => 
      if (f(h)) cons(h, t)
      else t) 
  
  def append[B>:A](s: => Stream[B]): Stream[B] = 
    foldRight(s)((h,t) => cons(h,t))
  
  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((h,t) => f(h) append t)

  def mapViaUnfold[B](f: A => B): Stream[B] = 
    unfold(this) { 
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }
  
  def takeViaUnfold(n: Int): Stream[A] = 
    unfold((this,n)) { 
      case (Cons(h,t), n) if n > 0 => Some((h(), (t(), n-1)))
      case _ => None
    }
  
  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = 
    unfold(this) { 
      case Cons(h,t) =>
        val head = h()
        if (f(head)) Some((head, t())) else None
      case _ => None
    }
  
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = 
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) => 
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  
  // special case of `zip`
  def zip[B](s2: Stream[B]): Stream[(A,B)] = 
    zipWith(s2)((_,_))
  
  
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = 
    zipWithAll(s2)((_,_))
  
  /* 
  There are a number of edge cases with this function. We can deal with some of these edge cases by treating each stream as an infinite series of `Option` values, which become `None` when the stream is exhausted. 
  */
  def zipWithAll[B,C](s2: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] = {
    val a = this map (Some(_)) append (constant(None)) 
    val b = s2 map (Some(_)) append (constant(None)) 
    unfold((a, b)) {
      case (Empty, Empty) => None
      case (s1, s2) => for {
        h1 <- s1.headOption
        h2 <- s2.headOption
      } yield (f(h1,h2), (s1 drop 1, s2 drop 1))
    }
  }

  /* 
  `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness, we can compose these three separate logical steps--the zipping, the termination when the second stream is exhausted, and the termination if a nonmatching element is found or the first stream is exhausted.
  */
  def startsWith[A](s: Stream[A]): Boolean = 
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  /*
  The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
  */
  def tails: Stream[Stream[A]] = 
    unfold(this) { 
      case Empty => None
      case s => Some((s, s drop 1))
    } append (Stream(empty))

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  /*
  The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.
  
  The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
  */
  def scanRight[B](z: B)(f: (A,=>B) => B): Stream[B] = 
    foldRight((z, Stream(z)))((a,p) => {
      val b2 = f(a,p._1)
      (b2, cons(b2,p._2))
    })._2

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) =>
      val head = h()
      if (f(head)) Some(head) else t().find(f)
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

  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail) 
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

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
  val fibsViaUnfold = 
    unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }
  
  def fromViaUnfold(n: Int) = 
    unfold(n)(n => Some((n,n+1)))
  
  def constantViaUnfold[A](a: A) = 
    unfold(a)(_ => Some((a,a)))
  
  // could also of course be implemented as constant(1)
  val onesViaUnfold = unfold(1)(_ => Some((1,1)))
}

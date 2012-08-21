package fpinscala.laziness

import Stream._

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  /* 
  The natural recursive solution will stack overflow for large streams. We'll write it with an explicit loop instead. 
  */
  def toList: List[A] = {
    val buf = new collection.mutable.ListBuffer[A] 
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s uncons match {
      case Some((h,t)) => buf += h; go(t)
      case _ => buf.toList
    }
    go(this)
  }
  def toListRecursive: List[A] = uncons match {
    case Some((h,t)) => h :: t.toList
    case _ => List()
  }

  def take(n: Int): Stream[A] = uncons match {
    case Some((h,t)) if n > 0 => cons(h, t.take(n-1)) 
    case _ => Stream()
  }

  /*
  It's a common Scala style to write method calls without `.` notation, as in `t takeWhile f`.  
  */
  def takeWhile(f: A => Boolean): Stream[A] = uncons match { 
    case Some((h,t)) if f(h) => cons(h, t takeWhile f)
    case _ => empty 
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

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
    unfold(this)(_.uncons.map { case (h,t) => (f(h), t) })
  
  def mapViaUnfold_1[B](f: A => B): Stream[B] = 
    unfold(this)(_.uncons match {
      case None => None
      case Some((h,t)) => Some((f(h), t))
    })
  
  def takeViaUnfold(n: Int): Stream[A] = 
    unfold((this,n)) { 
      case (s,n) if n > 0 => 
        s.uncons.map { case (h,t) => (h, (t,n)) }
      case _ => None
    }
  
  /*
  Notice we are using a _pattern label_ here. In front of any pattern, `x`, we can say `labelName@x`, to introduce a variable we can reference on the right-hand side. Here we are giving `Some((h,_))` a name to allow us to simply reuse the value (rather than reconstructing it) if the guard (in this case `if f(h)`) passes. 
  */
  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = 
    unfold(this)(s => s.uncons match { 
      case s@Some((h,_)) if f(h) => s
      case _ => None
    })
  
  def zip[B](s2: Stream[B]): Stream[(A,B)] = 
    zipWith(s2)((_,_))
  
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = 
    unfold((this, s2)) { case (s1,s2) => 
      (s1.uncons, s2.uncons) match {
        case (Some((h1,t1)), Some((h2,t2))) => Some((f(h1,h2), (t1,t2)))
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
      case (s1,s2) if s1.isEmpty && s2.isEmpty => None
      case (s1,s2) => {
        val (h1,t1) = s1.uncons.get 
        val (h2,t2) = s2.uncons.get
        Some((f(h1,h2), (t1,t2)))
      }
    }
  }

  /*
  The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
  */
  def tails: Stream[Stream[A]] = 
    unfold(this)(s => s.uncons match {
      case None => None 
      case Some((_,t)) => Some((s,t))
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

object Stream {
  def empty[A]: Stream[A] = 
    new Stream[A] { def uncons = None }
  
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = 
    new Stream[A] {
      lazy val uncons = Some((hd, tl)) 
    }
  
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = new Stream[A] {
    def uncons = Some((a, this))
  }

  def from(n: Int): Stream[Int] = new Stream[Int] {
    def uncons = Some((n, from(n+1)))
  }

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] = 
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,t)) => cons(h, unfold(t)(f))
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
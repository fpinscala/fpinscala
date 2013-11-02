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
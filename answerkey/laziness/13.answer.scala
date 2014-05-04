def mapViaUnfold[B](f: A => B): Stream[B] = 
  unfold(this) { 
    case Cons(h,t) => Some((f(h()), t()))
    case _ => None
  }

def takeViaUnfold(n: Int): Stream[A] =
  unfold((this,n)) {
    case (Cons(h,t), n) if n == 1 => Some((h(), (empty, n-1)))
    case (Cons(h,t), n) if n > 0 => Some((h(), (t(), n-1)))
    case _ => None
  }

def takeWhileViaUnfold(f: A => Boolean): Stream[A] = 
  unfold(this) { 
    case Cons(h,t) if f(h()) => Some((h(), t()))
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
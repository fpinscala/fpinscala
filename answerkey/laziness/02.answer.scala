
/*
`take` first checks if n==0. In that case we need not look at the stream at all.
*/
def take(n: Int): Stream[A] =
  if (n > 0) this match {
    case Cons(h, t) if n == 1 => cons(h(), Stream.empty) // we can say Stream.empty
    case Cons(h, t) => cons(h(), t().take(n-1))
    case _ => Stream.empty
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
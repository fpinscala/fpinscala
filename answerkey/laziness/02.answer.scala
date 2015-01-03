
/*
`take` first checks if n==0. In that case we need not look at the stream at all.
*/
def take(n: Int): Stream[A] = this match {
  case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
  case Cons(h, _) if n == 1 => cons(h(), empty)
  case _ => empty
}


/*
Unlike `take`, `drop` is not incremental. That is, it doesn't generate the
answer lazily. It must traverse the first `n` elements of the stream eagerly.
*/
@annotation.tailrec
final def drop(n: Int): Stream[A] = this match {
  case Cons(_, t) if n > 0 => t().drop(n - 1)
  case _ => this
}


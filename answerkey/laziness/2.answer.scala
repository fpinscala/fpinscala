
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
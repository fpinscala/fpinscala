def hasSubsequence[A](s: Stream[A]): Boolean =
  tails exists (_ startsWith s)
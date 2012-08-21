def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
  s1.tails exists (startsWith(_,s2))
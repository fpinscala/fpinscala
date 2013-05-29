/* This implementation uses `get` directly and does not propagate timeouts. */
def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
  es => if (a(es).get) ifTrue(es) else ifFalse(es)
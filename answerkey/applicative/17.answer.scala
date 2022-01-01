extension [A](fa: F[A])
  override def foldLeft[B](acc: B)(f: (B, A) => B): B =
    fa.mapAccum(acc)((a, b) => ((), f(b, a)))(1)
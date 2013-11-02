override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
  mapAccum(fa, z)((a, b) => ((), f(b, a)))._2
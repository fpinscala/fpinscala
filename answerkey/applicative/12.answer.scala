def reverse[A](fa: F[A]): F[A] =
  mapAccum(fa, toList(fa))((_, as) => (as.head, as.tail))._1
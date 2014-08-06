def reverse[A](fa: F[A]): F[A] =
  mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

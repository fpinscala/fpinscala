extension [A](fa: F[A])
  def reverse: F[A] =
    fa.mapAccum(fa.toList.reverse)((_, as) => (as.head, as.tail))(0)

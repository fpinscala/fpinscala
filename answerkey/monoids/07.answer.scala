def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
  if (as.length == 0)
    m.zero
  else if (as.length == 1)
    f(as(0))
  else {
    val (l, r) = as.splitAt(as.length / 2)
    m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
  }
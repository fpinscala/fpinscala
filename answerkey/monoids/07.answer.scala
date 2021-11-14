def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
  if as.length == 0 then
    m.empty
  else if as.length == 1 then
    f(as(0))
  else
    val (l, r) = as.splitAt(as.length / 2)
    m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))
// This ability to 'lift' a monoid any monoid to operate within
// some context (here `Par`) is something we'll discuss more in 
// chapters 11 & 12
def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
  def combine(a1: Par[A], a2: Par[A]) = a1.map2(a2)(m.combine)
  val empty = Par.unit(m.empty)

// we perform the mapping and the reducing both in parallel
def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
  Par.parMap(as)(f).flatMap(bs =>
    foldMapV(bs, par(m))(b => Par.lazyUnit(b))
  )
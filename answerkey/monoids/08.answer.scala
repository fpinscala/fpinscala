// This ability to 'lift' a monoid any monoid to operate within
// some context (here `Par`) is something we'll discuss more in 
// chapters 11 & 12
def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
  def zero = Par.unit(m.zero)  
  def op(a: Par[A], b: Par[A]) = a.map2(b)(m.op)
}

// we perform the mapping and the reducing both in parallel
def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
  Par.parMap(v)(f).flatMap { bs => 
    foldMapV(bs, par(m))(b => Par.async(b)) 
  }
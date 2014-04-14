def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
  new Monoid[(A, B)] {
    def op(x: (A, B), y: (A, B)) =
      (A.op(x._1, y._1), B.op(x._2, y._2))
    val zero = (A.zero, B.zero)
  }
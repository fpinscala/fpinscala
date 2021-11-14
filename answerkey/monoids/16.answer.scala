given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
  def combine(x: (A, B), y: (A, B)) =
    (ma.combine(x(0), y(0)), mb.combine(x(1), y(1)))
  val empty = (ma.empty, mb.empty)

given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
  def combine(f: A => B, g: A => B) = a => mb.combine(f(a), g(a))
  val empty: A => B = a => mb.empty
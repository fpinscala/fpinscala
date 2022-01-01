trait Applicative[F[_]] extends Functor[F]:
  self =>
  def compose[G[_]](G: Applicative[G]): Applicative[[X] =>> F[G[X]]] = new:
    def unit[A](a: => A) = self.unit(G.unit(a))
    extension [A](fga: F[G[A]])
      override def map2[B, C](fgb: F[G[B]])(f: (A, B) => C) =
        self.map2(fga)(fgb)(G.map2(_)(_)(f))

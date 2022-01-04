trait Applicative[F[_]] extends Functor[F]:
  self =>
  def product[G[_]](G: Applicative[G]): Applicative[[X] =>> (F[X], G[X])] = new:
    def unit[A](a: => A) = (self.unit(a), G.unit(a))
    override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
      (self.apply(fs(0))(p(0)), G.apply(fs(1))(p(1)))

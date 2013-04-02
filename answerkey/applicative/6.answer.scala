def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
  val self = this
  new Applicative[({type f[x] = (F[x], G[x])})#f] {
    def unit[A](a: => A) = (self.unit(a), G.unit(a))
    override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
      (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
  }
}
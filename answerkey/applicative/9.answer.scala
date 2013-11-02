def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
  val self = this
  new Applicative[({type f[x] = F[G[x]]})#f] {
    def unit[A](a: => A) = self.unit(G.unit(a))
    override def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C) =
      self.map2(fga, fgb)(G.map2(_,_)(f))
  }
}
// If `self` and `G` both satisfy the laws, then so does the composite.
// The full solution can be found at:
// https://github.com/runarorama/sannanir/blob/master/Applicative.v
def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
  new Traverse[({type f[x] = F[G[x]]})#f] {
    override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) =
      self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
  }
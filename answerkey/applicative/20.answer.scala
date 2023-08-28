def composeM[F[_],G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]):
  Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
    def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
    override def flatMap[A,B](mna: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
      F.flatMap(mna)(na => F.map(T.traverse(na)(f))(G.join))
  }

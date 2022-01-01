def composeM[G[_], H[_]](using G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[[X] =>> G[H[X]]] = new:
  def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
  extension [A](gha: G[H[A]])
    override def flatMap[B](f: A => G[H[B]]): G[H[B]] =
      G.flatMap(gha)(ha => G.map(T.traverse(ha)(f))(H.join))  

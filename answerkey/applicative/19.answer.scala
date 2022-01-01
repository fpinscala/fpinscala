def compose[G[_]: Traverse]: Traverse[[X] =>> F[G[X]]] = new:
  extension [A](fga: F[G[A]])
    override def traverse[H[_]: Applicative, B](f: A => H[B]): H[F[G[B]]] =
      self.traverse(fga)(ga => ga.traverse(f))

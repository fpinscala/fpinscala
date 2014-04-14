def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                       (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
  traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)
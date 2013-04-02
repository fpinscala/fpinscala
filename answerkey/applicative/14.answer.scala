def fuse[M[_],N[_],A,B](fa: F[A])(f: A => M[B], g: A => N[B])
                       (implicit M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) =
  traverse[({type f[x] = (M[x], N[x])})#f, A, B](fa)(a => (f(a), g(a)))(M product N)
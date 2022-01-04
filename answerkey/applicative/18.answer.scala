extension [A](fa: F[A])
  def fuse[M[_], N[_], B](
    f: A => M[B], g: A => N[B])(using m: Applicative[M], n: Applicative[N]
  ): (M[F[B]], N[F[B]]) =
      fa.traverse[[X] =>> (M[X], N[X]), B](a => (f(a), g(a)))(using m.product(n))

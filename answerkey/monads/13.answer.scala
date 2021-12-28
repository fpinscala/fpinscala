extension [A](fa: F[A])
  def flatMapViaJoinAndMap[B](f: A => F[B]): F[B] =
    fa.map(f).join

def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
  a => f(a).map(g).join
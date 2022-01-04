def sequence[A](fas: List[F[A]]): F[List[A]] =
  traverse(fas)(fa => fa)

def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
  as.foldRight(unit(List[B]()))((a, acc) => f(a).map2(acc)(_ :: _))

def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
  sequence(List.fill(n)(fa))

extension [A](fa: F[A])
  def product[B](fb: F[B]): F[(A, B)] =
    fa.map2(fb)((_, _))

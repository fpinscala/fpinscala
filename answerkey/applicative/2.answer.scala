def sequence[A](fas: List[F[A]]): F[List[A]] =
  traverse(fas)(fa => fa)

def traverse[A](as: List[A])(f: A => F[B]): F[List[B]] =
  as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
  sequence(List.fill(n)(fa))

def factor[A,B](fa: F[A], fb: F[A]): F[(A,B)] =
  map2(fa, fb)((_,_))
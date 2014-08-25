def sequence[A](fas: List[F[A]]): F[List[A]] =
  traverse(fas)(fa => fa)

def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
  sequence(List.fill(n)(fa))

def product[A,B](fa: F[A], fb: F[A]): F[(A,B)] =
  map2(fa, fb)((_,_))
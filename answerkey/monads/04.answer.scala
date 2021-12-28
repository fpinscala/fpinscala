def replicateMRecursive[A](n: Int, fa: F[A]): F[List[A]] =
  if n <= 0 then unit(Nil)
  else fa.map2(replicateMRecursive(n - 1, fa))(_ :: _)

def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
  sequence(List.fill(n)(fa))

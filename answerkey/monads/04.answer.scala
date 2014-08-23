// Recursive version:
def _replicateM[A](n: Int, ma: F[A]): F[List[A]] =
  if (n <= 0) unit(List[A]()) else map2(ma, replicateM(n - 1, ma))(_ :: _)

// Using `sequence` and the `List.fill` function of the standard library:
def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
  sequence(List.fill(n)(ma))
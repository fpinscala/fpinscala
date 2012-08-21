def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A =
  if (p(a)) iterateWhile(f(a))(f, p) else a
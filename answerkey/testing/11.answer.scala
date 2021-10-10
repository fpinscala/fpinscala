extension [A](self: SGen[A]) def map[B](f: A => B): SGen[B] =
  n => self(n).map(f)

extension [A](self: SGen[A]) def flatMap[B](f: A => SGen[B]): SGen[B] =
  n => self(n).flatMap(a => f(a)(n))

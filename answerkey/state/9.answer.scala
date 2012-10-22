def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

def _positiveInt: Rand[Int] = {
  flatMap(int) { i =>
    if (i != Int.MinValue) unit(i.abs) else _positiveInt
  }
}
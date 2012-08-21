def absolute[A](f: A => Int): A => Int =
  a => f(a).abs
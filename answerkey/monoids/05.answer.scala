// Notice that this function does not require the use of `map` at all.
def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
  as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
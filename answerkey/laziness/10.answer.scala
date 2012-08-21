def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) match {
    case Some((h,t)) => cons(h, unfold(t)(f))
    case None => empty
  }
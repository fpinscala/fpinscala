def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case None => empty
  }
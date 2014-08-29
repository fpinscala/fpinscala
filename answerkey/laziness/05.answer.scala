def takeWhile_1(f: A => Boolean): Stream[A] =
  foldRight(empty[A])((h,t) => 
    if (f(h)) cons(h,t)
    else      empty)
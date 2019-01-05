 def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    parMap(as.filter(f))(identity)

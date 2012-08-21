def sequence[A](l: List[Par[A]]): Par[List[A]] = 
  l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))
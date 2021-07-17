def doubleToString(l: List[Double]): List[String] =
  foldRight(l, Nil:List[String], (h,t) => Cons(h.toString,t))
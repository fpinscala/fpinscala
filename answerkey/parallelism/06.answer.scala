 def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
   val p_as: List[Par[A]] = as.filter(f).map(asyncF(identity))
   sequence(p_as)
 }

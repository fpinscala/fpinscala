def take(n: Int): Stream[A] = uncons match {
  case Some((h,t)) if n > 0 => cons(h, t.take(n-1)) 
  case _ => Stream()
}
/*
It's a common Scala style to write method calls without `.` notation, as in `t() takeWhile f`.  
*/
def takeWhile(f: A => Boolean): Stream[A] = this match {
  case Cons(h,t) =>
    val head = h()
    if (f(head)) cons(head, t() takeWhile f) else empty
  case _ => empty
}
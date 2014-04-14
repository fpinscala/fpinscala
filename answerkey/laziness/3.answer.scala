/*
It's a common Scala style to write method calls without `.` notation, as in `t() takeWhile f`.  
*/
def takeWhile(f: A => Boolean): Stream[A] = this match { 
  case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
  case _ => empty 
}
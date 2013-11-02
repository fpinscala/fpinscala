/*
It's a common Scala style to write method calls without `.` notation, as in `c.tail takeWhile f`.  
*/
def takeWhile(f: A => Boolean): Stream[A] = uncons match { 
  case Some(c) if f(c.head) => cons(c.head, c.tail takeWhile f)
  case _ => empty 
}
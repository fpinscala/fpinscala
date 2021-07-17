/*
If a function body consists solely of a match expression, we'll often put the match on the same line as the
function signature, rather than introducing another level of nesting.
*/
def setHead[A](l: List[A], h: A): List[A] = l match
  case Nil => sys.error("setHead on empty list")
  case Cons(_,t) => Cons(h,t)

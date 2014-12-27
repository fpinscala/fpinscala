/*
There's nothing particularly bad about this implementation,
except that it's somewhat monolithic and easy to get wrong.
Where possible, we prefer to assemble functions like this using
combinations of other functions. It makes the code more obviously
correct and easier to read and understand. Notice that in this
implementation we need special purpose logic to break out of our
loops early. In Chapter 5 we'll discuss ways of composing functions
like this from simpler components, without giving up the efficiency
of having the resulting functions work in one pass over the data.
*/
@annotation.tailrec
def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
  case (_,Nil) => true
  case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
  case _ => false
}
@annotation.tailrec
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
  case Nil => false
  case _ if startsWith(sup, sub) => true
  case Cons(h,t) => hasSubsequence(t, sub)
}

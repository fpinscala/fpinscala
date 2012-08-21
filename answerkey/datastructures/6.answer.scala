/*
Notice we are copying the entire list up until the last element. Besides being inefficient, the natural recursive solution will stack overflow for large lists (can you see why?). With strict lists, it's more typical to use a temporary, mutable buffer internal to the function. So long as the buffer is allocated internal to the function, the mutation is not observable and RT is preserved.
*/
def init[A](l: List[A]): List[A] = 
  l match { 
    case Nil => sys.error("init of empty list")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }
def init2[A](l: List[A]): List[A] = {
  import collection.mutable.ListBuffer
  val buf = new ListBuffer[A]
  def go(cur: List[A]): List[A] = cur match {
    case Nil => sys.error("init of empty list")
    case Cons(_,Nil) => List(buf.toList: _*)
    case Cons(h,t) => buf += h; go(t)
  }
  go(l)
}
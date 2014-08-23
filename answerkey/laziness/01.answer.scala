// The natural recursive solution
def toListRecursive: List[A] = this match {
  case Cons(h,t) => h() :: t().toListRecursive
  case _ => List()
}

/*
The above solution will stack overflow for large streams, since it's
not tail-recursive. Here is a tail-recursive implementation. At each
step we cons onto the front of the `acc` list, which will result in the
reverse of the stream. Then at the end we reverse the result to get the
correct order again.
[:ben] are the line breaks above okay? I'm unclear on whether these "hints" are supposed to go in the book or not
*/
def toList: List[A] = {
  @annotation.tailrec
  def go(s: Stream[A], acc: List[A]): List[A] = s match {
    case Cons(h,t) => go(t(), h() :: acc)
    case _ => acc
  }
  go(this, List()).reverse
}

/* 
In order to avoid the `reverse` at the end, we could write it using a
mutable list buffer and an explicit loop instead. Note that the mutable
list buffer never escapes our `toList` method, so this function is
still _pure_.
*/
def toListFast: List[A] = {
  val buf = new collection.mutable.ListBuffer[A] 
  @annotation.tailrec
  def go(s: Stream[A]): List[A] = s match {
    case Cons(h,t) =>
      buf += h()
      go(t())
    case _ => buf.toList
  }
  go(this)
}
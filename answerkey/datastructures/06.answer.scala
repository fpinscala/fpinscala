/*
Note that we're copying the entire list up until the last element. Besides being inefficient, the natural recursive
solution will use a stack frame for each element of the list, which can lead to stack overflows for
large lists (can you see why?). With lists, it's common to use a temporary, mutable buffer internal to the
function (with lazy lists or streams, which we discuss in chapter 5, we don't normally do this). So long as the
buffer is allocated internal to the function, the mutation is not observable and RT is preserved.

Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which
doesn't require even local mutation. We'll write a reverse function later in this chapter.
*/
def init[A](l: List[A]): List[A] =
  l match
    case Nil => sys.error("init of empty list")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))

def init2[A](l: List[A]): List[A] =
  import collection.mutable.ListBuffer
  val buf = new ListBuffer[A]
  @annotation.tailrec
  def go(cur: List[A]): List[A] = cur match
    case Nil => sys.error("init of empty list")
    case Cons(_,Nil) => List(buf.toList*)
    case Cons(h,t) => buf += h; go(t)
  go(l)


/* 
A natural solution is using `foldRight`, but our implementation of `foldRight` is not stack-safe. We can use `foldRightViaFoldLeft` to avoid the stack overflow (variation 1), but more commonly, with our current implementation of `List`, `map` will just be implemented using local mutation (variation 2). Again, note that the mutation isn't observable outside the function, since we're only mutating a buffer that we've allocated. 
*/
def map[A,B](l: List[A])(f: A => B): List[B] = 
  foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

def map_1[A,B](l: List[A])(f: A => B): List[B] = 
  foldRightViaFoldLeft(l, Nil:List[B])((h,t) => Cons(f(h),t))

def map_2[A,B](l: List[A])(f: A => B): List[B] = {
  val buf = new collection.mutable.ListBuffer[B]
  def go(l: List[A]): Unit = l match {
    case Nil => ()
    case Cons(h,t) => buf += f(h); go(t)
  }
  go(l)
  List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
}
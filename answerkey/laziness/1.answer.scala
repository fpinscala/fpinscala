/* 
The natural recursive solution will stack overflow for large streams. We'll write it with an explicit loop instead. 
*/
def toList: List[A] = {
  val buf = new collection.mutable.ListBuffer[A] 
  @annotation.tailrec
  def go(s: Stream[A]): List[A] = s uncons match {
    case Some((h,t)) => buf += h; go(t)
    case _ => buf.toList
  }
  go(this)
}
def toListRecursive: List[A] = uncons match {
  case Some((h,t)) => h :: t.toList
  case _ => List()
}
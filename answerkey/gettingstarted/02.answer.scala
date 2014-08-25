def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(i: Int, prev: A): Boolean = 
    if (i == as.length) true
    else if (gt(as(i), prev)) go(i + 1, as(i))
    else false
  if (as.length == 0) true
  else go(1, as(0))
}
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(n: Int): Boolean =
    if (n >= as.length-1) true
    else if (!ordered(as(n), as(n+1))) false
    else go(n+1)

  go(0)
}
// This implementation detects only ascending order,
// but you can write a monoid that detects both ascending and descending
// order if you like.
def ordered(ints: IndexedSeq[Int]): Boolean = {
  // Our monoid tracks the minimum and maximum element seen so far
  // as well as whether the elements are so far ordered.
  val mon = new Monoid[Option[(Int, Int, Boolean)]] {
    def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
      (o1, o2) match {
        // The ranges should not overlap if the sequence is ordered.
        case (Some((x1, y1, p)), Some((x2, y2, q))) =>
          Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
        case (x, None) => x
        case (None, x) => x
      }
    val zero = None
  }
  // The empty sequence is ordered, and each element by itself is ordered.
  foldMapV(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
}
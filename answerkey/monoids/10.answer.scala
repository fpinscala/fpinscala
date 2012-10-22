def ordered(ints: IndexedSeq[Int]): Boolean = {
  val mon = new Monoid[Option[(Int, Boolean)]] {
    def op(o1: Option[(Int, Boolean)], o2: Option[(Int, Boolean)]) =
      (o1, o2) match {
        case (Some((n, p)), Some((m, q))) =>
          val b = n <= m
          Some((if (p || b) n else m, p && b && q))
        case (x, None) => x
        case (None, x) => x
      }
    val zero = None
  }
  foldMapV(ints, mon)(i => Some((i, true))).map(_._2).getOrElse(true)
}



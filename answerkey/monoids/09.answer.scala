val orderedMonoid: Monoid[(Boolean, Int)] = new:
  def combine(a1: (Boolean, Int), a2: (Boolean, Int)) =
    (a1(0) && a2(0) && a1(1) <= a2(1), a1(1) max a2(1))
  val empty = (true, Int.MinValue)

def ordered(ints: IndexedSeq[Int]): Boolean =
  foldMapV(ints, orderedMonoid)(i => (true, i))(0)

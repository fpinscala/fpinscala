given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
  def combine(a: Map[K, V], b: Map[K, V]) =
    (a.keySet ++ b.keySet).foldLeft(empty) { (acc,k) =>
      acc.updated(k, mv.combine(a.getOrElse(k, mv.empty),
                                b.getOrElse(k, mv.empty)))
    }
  val empty = Map()

def bag[A](as: IndexedSeq[A]): Map[A, Int] =
  import Foldable.given
  as.foldMap(a => Map(a -> 1))

def bagManualComposition[A](as: IndexedSeq[A]): Map[A, Int] =
  val bagMonoid = mapMergeMonoid[A, Int](using intAddition)
  foldMapV(as, bagMonoid)(a => Map(a -> 1))

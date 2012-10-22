def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
  new Monoid[Map[K, V]] {
    def zero = Map()
    def op(a: Map[K, V], b: Map[K, V]) =
      a.map {
        case (k, v) => (k, V.op(v, b.get(k) getOrElse V.zero))
      }
  }

def bag[A](as: IndexedSeq[A]): Map[A, Int] =
  foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))

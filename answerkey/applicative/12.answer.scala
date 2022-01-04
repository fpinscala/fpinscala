def sequenceMap[K, V](ofv: Map[K, F[V]]): F[Map[K, V]] =
  ofv.foldLeft(unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
    acc.map2(fv)((m, v) => m + (k -> v))
  }

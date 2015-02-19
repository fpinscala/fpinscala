def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    (ofa foldLeft unit(Map.empty[K,V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }

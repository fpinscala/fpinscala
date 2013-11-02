def sequenceMap[F[_],K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
  ofa.foldLeft(unit(Map()))((acc, (k, fv)) =>
    apply(map(acc)(m => n => m ++ n))(map(fv)(Map(k -> _))))
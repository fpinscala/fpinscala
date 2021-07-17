def length[A](l: List[A]): Int =
  foldRight(l, 0, (_,acc) => acc + 1)
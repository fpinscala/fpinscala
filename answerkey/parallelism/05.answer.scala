def sequence_simple[A](l: List[Par[A]]): Par[List[A]] = 
  l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

// This implementation forks the recursive step off to a new logical thread,
// making it effectively tail-recursive. However, we are constructing
// a right-nested parallel program, and we can get better performance by 
// dividing the list in half, and running both halves in parallel. 
// See `sequenceBalanced` below.
def sequenceRight[A](as: List[Par[A]]): Par[List[A]] = 
  as match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
  }

// We define `sequenceBalanced` using `IndexedSeq`, which provides an 
// efficient function for splitting the sequence in half. 
def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
  if (as.isEmpty) unit(Vector())
  else if (as.length == 1) map(as.head)(a => Vector(a))
  else {
    val (l,r) = as.splitAt(as.length/2)
    map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
  }
}

def sequence[A](as: List[Par[A]]): Par[List[A]] =
  map(sequenceBalanced(as.toIndexedSeq))(_.toList)

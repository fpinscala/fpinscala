def sequenceSimple[A](pas: List[Par[A]]): Par[List[A]] =
  pas.foldRight(unit(List.empty[A]))((pa, acc) => pa.map2(acc)(_ :: _))

// This implementation forks the recursive step off to a new logical thread,
// making it effectively tail-recursive. However, we are constructing
// a right-nested parallel program, and we can get better performance by
// dividing the list in half, and running both halves in parallel.
// See `sequenceBalanced` below.
def sequenceRight[A](pas: List[Par[A]]): Par[List[A]] =
  pas match
    case Nil => unit(Nil)
    case h :: t => h.map2(fork(sequenceRight(t)))(_ :: _)

// We define `sequenceBalanced` using `IndexedSeq`, which provides an
// efficient function for splitting the sequence in half.
def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
  if pas.isEmpty then unit(IndexedSeq.empty)
  else if pas.size == 1 then pas.head.map(a => IndexedSeq(a))
  else
    val (l, r) = pas.splitAt(pas.size / 2)
    sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)

def sequence[A](pas: List[Par[A]]): Par[List[A]] =
  sequenceBalanced(pas.toIndexedSeq).map(_.toList)
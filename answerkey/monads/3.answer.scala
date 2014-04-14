def sequence[A](lma: List[F[A]]): F[List[A]] =
  lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
  la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

/** 
 * 'Balanced' sequencing, which should behave like `sequence`,
 * but it can use less stack for some data types. We'll see later
 * in this chapter how the monad _laws_ let us conclude both 
 * definitions 'mean' the same thing.
 */
def bsequence[A](ms: Seq[F[A]]): F[IndexedSeq[A]] = {
  if (ms.isEmpty) point(Vector())
  else if (ms.size == 1) ms.head.map(Vector(_))
  else {
    val (l,r) = ms.toIndexedSeq.splitAt(ms.length / 2)
    map2(bsequence(l), bsequence(r))(_ ++ _)
  }
}
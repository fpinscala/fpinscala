/* The simplest possible implementation. This will put all elements of one
 * `Gen` before the other in the exhaustive traversal. It might be nice to
 * interleave the two streams, so we get a more representative sample if we
 * don't get to examine the entire exhaustive stream. 
 */
def union_1[A](g1: Gen[A], g2: Gen[A]): Gen[A] = 
  boolean.flatMap(b => if (b) g1 else g2) 

def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = 
  Gen(
    State(RNG.boolean).flatMap(b => if (b) g1.sample else g2.sample),
    interleave(g1.exhaustive, g2.exhaustive)
  )

def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = 
  s1.zipAll(s2).flatMap { case (a,a2) => Stream((a.toList ++ a2.toList): _*) }
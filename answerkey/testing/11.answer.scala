/* The random case is simple - we generate a double and use this to choose between
 * the two random samplers. The exhaustive case is trickier if we want to try
 * to produce a stream that does a weighted interleave of the two exhaustive streams.
 */
def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
  /* The probability we should pull from `g1`. */
  val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

  /* Some random booleans to use for selecting between g1 and g2 in the exhaustive case.
   * Making up a seed locally is fine here, since we just want a deterministic schedule
   * with the right distribution. */
  def bools: Stream[Boolean] = 
    randomStream(uniform.map(_ < g1Threshold))(RNG.simple(302837L))

  Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample),
      interleave(bools, g1._1.exhaustive, g2._1.exhaustive)) 
}

/* Produce an infinite random stream from a `Gen` and a starting `RNG`. */ 
def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = 
  Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

/* Interleave the two streams, using `b` to control which stream to pull from at each step. 
 * A value of `true` attempts to pull from `s1`; `false` attempts to pull from `s1`. 
 * When either stream is exhausted, insert all remaining elements from the other stream.
 */
def interleave[A](b: Stream[Boolean], s1: Stream[A], s2: Stream[A]): Stream[A] = new Stream[A] {
  def uncons = b.uncons flatMap { case (bh,bt) => 
    if (bh) s1.uncons map { case (s1h,s1t) => (s1h, interleave(bt,s1t,s2)) } orElse s2.uncons 
    else s2.uncons map { case (s2h,s2t) => (s2h, interleave(bt,s1,s2t)) } orElse s1.uncons 
  }
}
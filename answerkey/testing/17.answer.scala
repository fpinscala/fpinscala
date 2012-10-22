/* We pattern match on the `SGen`, and delegate to our `Gen` version of `forAll`
 * if `g` is unsized; otherwise, we call the sized version of `forAll` (below). 
 */
def forAll[A](g: SGen[A])(f: A => Boolean): Prop = g match {
  case Unsized(g2) => forAll(g2)(f) 
  case Sized(gs) => forAll(gs)(f)
}

/* The sized case of `forAll` is as before, though we convert from `Proven` to 
 * `Exhausted`. A sized generator can never be proven, since there are always
 * larger-sized tests that were not run which may have failed.
 */
def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
  (max,n,rng) => 
    val casesPerSize = n / max + 1
    val props: List[Prop] = 
      Stream.from(0).take(max+1).map(i => forAll(g(i))(f)).toList
    val p: Prop = props.map(p => Prop((max,n,rng) => p.run(max,casesPerSize,rng))).
          reduceLeft(_ && _)
    p.run(max,n,rng).right.map { 
      case (Proven,n) => (Exhausted,n)
      case x => x
    }
}
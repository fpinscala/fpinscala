// Here we simply use `map2` to lift `apply` and `unit` themselves from one Applicative into the other.
def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
  val self = this
  new Applicative[({type f[x] = F[G[x]]})#f] {
    def unit[A](a: => A) = self.unit(G.unit(a))
    def apply[A,B](fgf: F[G[A => B]])(fga: F[G[A]]) =
      self.map2(fgf, fga)(G.apply(_)(_))
  }
}

// If `self` and `G` both satisfy the laws, then so does the composite.
// The full solution can be found at
// https://github.com/runarorama/sannanir/blob/master/Applicative.v
// 
// -- Identity is simple: 
// apply(unit(x => x))(v) == v
// -- Expand definitions of `apply` and `unit`:
// F.map2(F.unit(G.unit(x => x)), v)(G.apply(_)(_)) == v
// -- Definition of map2:
// F.apply(F.map(F.unit(G.unit(x => x)))(G.apply))(v) == v
// -- Homomorphism for F:
// F.apply(F.unit(y => G.apply(G.unit(x => x))(y)))(v) == v
// -- Assume identity law for G:
// F.apply(F.unit(y => y))(v) == v
// -- Assume identity law for F:
// v == v
//
// -- Homomorphism is even simpler:
// apply(unit(f))(unit(x)) = unit(f(x))
// -- expand definitions of `apply` and `unit`, then `map2` and `map`:
// apply(apply(unit(apply))(unit(unit(f))))(unit(unit(x))) = unit(unit(f(x)))
// -- repeatedly rewrite using homomorphism law for F and G.
// unit(unit(f(x))) == unit(unit(f(x)))
//
// -- Interchange takes much longer, so we will show only the steps:
//    0. Expand definitions of `apply` and `unit`, then `map2` and `map`:
//    1. Rewrite using homomorphism law.
//    2. Rewrite using interchange law on both sides, in opposite directions.
//    3. Rewrite using composition law.
//    4. Rewrite using homomorphism law again.
//    5. Rewrite using interchange law.
//    6. Rewrite using homomorphism law.
//    7. Expand the definition of function composition.
//
// The proof for the composition law is the longest since it's the
// law with the most content.
// Here's a strategy for finding a proof:
//    * Expand definitions of `apply`, `unit`, `map2`, and `map`.
//    * Repeat the following as often as possible:
//      * Simplify with homomorphism if possible.
//      * If homomorphism does not apply, rewrite using the composition law
//        to introduce terms that can be eliminated with homomorphism.
//    * Now all variables should be "on the outside".
//    * Rewrite once using the interchange law.
//    * Rewrite once using the composition law and simplify with homomorphism.
//    * Expand the definition of `compose`.
//    * This should allow you to collapse `compose` again a different way.
//    * Now rewrite with composition and simplify with homomorphism.
//    * Rewriting with composition one last time, you should end up with
//      a function on both sides of the equals sign.
//    * Assuming the "axiom of extensionality" (that functions are equal if they
//      return equal values for equal inputs), these two functions are identical.
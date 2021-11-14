// The function type `(A, B) => B`, when curried, is `A => (B => B)`.
// And of course, `B => B` is a monoid for any `B` (via function composition).
def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
  foldMap(as, endoMonoid)(f.curried)(acc)

// Folding to the left is the same except we flip the arguments to
// the function `f` to put the `B` on the correct side.
// Then we have to also "flip" the monoid so that it operates from left to right.
def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
  foldMap(as, dual(endoMonoid))(a => b => f(b, a))(acc)

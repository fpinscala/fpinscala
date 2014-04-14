// Notice that we have a choice in how we implement `op`.
// We can compose the options in either order. Both of those implementations
// satisfy the monoid laws, but they are not equivalent.
// This is true in general--that is, every monoid has a _dual_ where the
// `op` combines things in the opposite order. Monoids like `booleanOr` and
// `intAddition` are equivalent to their duals because their `op` is commutative
// as well as associative.

def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
  def op(x: Option[A], y: Option[A]) = x orElse y
  val zero = None
}

// We can get the dual of any monoid just by flipping the `op`.
def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
  def op(x: A, y: A): A = m.op(y, x)
  val zero = m.zero
}

// Now we can have both monoids on hand:
def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)
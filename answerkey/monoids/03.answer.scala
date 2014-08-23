// There is a choice of implementation here as well.
// Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
// We can then get the other one using the `dual` construct (see previous answer).
def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  def op(f: A => A, g: A => A) = f compose g
  val zero = (a: A) => a
}
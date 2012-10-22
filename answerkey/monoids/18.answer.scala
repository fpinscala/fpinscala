// There is no correct `Either` monoid. The trouble lies in providing a `zero`.
// Should the `zero` be a `Left` or a `Right`? Also, what happens when we combine
// both a `Left` and a `Right`? It's not possible to make an arbitrary decision
// about that and still satisfy the monoid laws.
// But it is possible to define a monoid coproduct using a slightly different
// data structure:

sealed trait These[+A, +B]
case class This[A](a: A) extends These[A, Nothing]
case class That[B](b: B) extends These[Nothing, B]
case class Both[A, B](a: A, b: B) extends These[A, B]

// This handily solves the issue of "both left and right" as well as the issue of
// which `zero` to choose. We simply use both of them.

object These {
  def theseMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[These[A, B]] =
    new Monoid[These[A, B]] {
      def op(x: These[A, B], y: These[A, B]) = (x, y) match {
        case (This(a1), This(a2)) => This(A.op(a1, a2))
        case (That(b1), That(b2)) => That(B.op(b1, b2))
        case (That(b), This(a)) => Both(a, b)
        case (This(a), That(b)) => Both(a, b)
        case (Both(a1, b), This(a)) => Both(A.op(a1, a), b)
        case (Both(a, b1), That(b)) => Both(a, B.op(b1, b))
        case (This(a1), Both(a, b)) => Both(A.op(a1, a), b)
        case (That(b), Both(a, b1)) => Both(a, B.op(b1, b))
        case (Both(a1, b1), Both(a2, b2)) => Both(A.op(a1, a2), B.op(b1, b2))
      }
      val zero = Both(A.zero, B.zero)
    }
}

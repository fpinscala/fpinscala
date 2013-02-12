// It turns out that `cofactor` can be implemented with just `map`.
// We did not need the full power of monads in this case. It's important
// to note that we are just playing here, and sometimes when playing we
// discover something unexpected. This method rightly belongs on `Functor`.
// What it does: It takes _one_ value, either of type M[A] or M[B] and returns
// that same value except with the value(s) inside wrapped in Left or Right
// according to whether the argument was Left or Right. I.e. it moves a Left
// or Right constructor from the outside to the inside of an M.
def cofactor[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
  case Left(ma) => map(ma)(Left(_))
  case Right(mb) => map(mb)(Right(_))
}
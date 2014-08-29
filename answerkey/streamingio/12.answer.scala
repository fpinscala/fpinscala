/* Exercise 12: Implement `join`. Notice this is the standard monadic combinator! */
def join[F[_],A](p: Process[F,Process[F,A]]): Process[F,A] =
  p.flatMap(pa => pa)

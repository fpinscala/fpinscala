/* Note that `=>` associates to the right, so we could write the return type as
   `A => B => C` */
def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  a => b => f(a, b)

/* NB: The `Function2` trait has a `curried` method already, so if you wanted to
   cheat a little you could write the answer as f.curried */
// You want to try writing `flatMap` in terms of `Monad[F]` and `Monad[G]`.

def flatMap[A,B](mna: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
  self.flatMap(na => G.flatMap(na)(a => ???))

// Here all you have is `f`, which returns an `F[G[B]]`. For it to have the appropriate type to return from the argument to `G.flatMap`, you'd need to be able to "swap" the `F` and `G` types. In other words, you'd need a _distributive law_. Such an operation is not part of the `Monad` interface.
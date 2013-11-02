You want to try writing `flatMap` in terms of `Monad[F]` and `Monad[N]`.

def flatMap[A,B](mna: F[N[A]])(f: A => F[N[B]]): F[N[B]] =
  self.flatMap(na => N.flatMap(na)(a => ???))

Here all you have is `f`, which returns an `F[G[B]]`. For it to have the appropriate type to return from the argument to `G.flatMap`, you would need to be able to "swap" the `F` and `G` types. In other words, you would need a _distributive law_. Such an operation is not part of the `Monad` interface.
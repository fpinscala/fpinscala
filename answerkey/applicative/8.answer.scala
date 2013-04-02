You want to try writing `flatMap` in terms of `Monad[M]` and `Monad[N]`.

def flatMap[A,B](mna: M[N[A]])(f: A => M[N[B]]): M[N[B]] =
  self.flatMap(na => N.flatMap(na)(a => ???))

Here all you have is `f`, which returns an `M[N[B]]`. For it to have the appropriate type to return from the argument to `N.flatMap`, you would need to be able to "swap" the `M` and `N` types. In other words, you would need a _distributive law_. Such an operation is not part of the `Monad` interface.
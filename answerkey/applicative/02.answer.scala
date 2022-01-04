trait Applicative[F[_]] extends Functor[F]:

  def unit[A](a: => A): F[A]

  // We simply use `map2` to lift a function into `F` so we can apply it
  // to both `fab` and `fa`. The function being lifted here is `_(_)`,
  // which is the same as the lambda notation `(f, x) => f(x)`. That is,
  // It's a function that takes two arguments:
  //   1. A function `f`
  //   2. An argument `x` to that function
  // and it simply applies `f` to `x`.
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    fab.map2(fa)(_(_))

  extension [A](fa: F[A])
    def map[B](f: A => B): F[B] =
      apply(unit(f))(fa)

    // `map2` is implemented by first currying `f` so we get a function
    // of type `A => B => C`. This is a function that takes `A` and returns
    // another function of type `B => C`. So if we map `f.curried` over an
    // `F[A]`, we get `F[B => C]`. Passing that to `apply` along with the
    // `F[B]` will give us the desired `F[C]`.
    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

// The simplest possible `Applicative` we can use is `Id`:

    type Id[A] = A

// We already know this forms a `Monad`, so it's also an applicative functor:

    given idMonad: Monad[Id] with
      def unit[A](a: => A) = a
      extension [A](a: A)
        override def flatMap[B](f: A => B): B = f(a)

// We can now implement `map` by calling `traverse`, picking `Id` as the `Applicative`:

    def map[B](f: A => B): F[B] =
      traverse[Id, B](f)(using idMonad)

// This implementation is suggestive of laws for `traverse`, since we expect this implementation to obey the usual functor laws. See the chapter notes for discussion of the laws for `Traverse`.

// Note that we can define `traverse` in terms of `sequence` and `map`, which means that a valid `Traverse` instance may define `sequence` and `map`, or just `traverse`: 

    trait Traverse[F[_]] extends Functor[F]:
      extension [A](fa: F[A])
        def traverse[G[_]:Applicative, B](f: A => G[B]): G[F[B]] =
          fa.map(f).sequence
        def map[B](f: A => B): F[B] =
          fa.traverse[Id, B](f)(using idMonad)
      extension [G[_]: Applicative, A](fga: F[G[A]])
        def sequence: G[F[A]] =
          traverse(fga)(ga => ga)

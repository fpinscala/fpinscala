The simplest possible `Applicative` we can use is `Id`:

    type Id[A] = A

We already know this forms a `Monad`, so it's also an applicative functor:

    val idMonad = new Monad[Id] {
      def unit[A](a: => A) = a
      override def flatMap[A,B](a: A)(f: A => B): B = f(a)
    }

We can now implement `map` by calling `traverse`, picking `Id` as the `Applicative`:

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      traverse[Id, A, B](xs)(f)(idMonad)

This implementation is suggestive of laws for `traverse`, since we expect this implementation to obey the usual functor laws. See the chapter notes for discussion of the laws for `Traverse`.

Note that we can define `traverse` in terms of `sequence` and `map`, which means that a valid `Traverse` instance may define `sequence` and `map`, or just `traverse`: 

    trait Traverse[F[_]] extends Functor[F] {
      def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
        sequence(map(fa)(f))
      def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
        traverse(fga)(ga => ga)
      def map[A,B](fa: F[A])(f: A => B): F[B] =
        traverse[Id, A, B](fa)(f)(idMonad)
    }
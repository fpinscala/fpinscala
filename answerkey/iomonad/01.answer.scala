  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] =
    new Monad[({type f[a] = Free[F,a]})#f] {
      def unit[A](a: => A) = Return(a)
      def flatMap[A,B](fa: Free[F, A])(f: A => Free[F, B]) = fa flatMap f
    }

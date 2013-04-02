def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
  new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)
    def flatMap[A,B](eea: Either[E, A])(f: A => Either[E, B]) = eea match {
      case Right(a) => f(a)
      case l => l
    }
  }
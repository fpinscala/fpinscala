given eitherMonad[E]: Monad[Either[E, _]] with
  def unit[A](a: => A): Either[E, A] = Right(a)
  extension [A](fa: Either[E, A])
    def flatMap[B](f: A => Either[E, B]): Either[E, B] =
      fa match
        case Right(a) => f(a)
        case Left(e) => Left(e)
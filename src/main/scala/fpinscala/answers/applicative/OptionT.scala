package fpinscala.answers.applicative

opaque type OptionT[F[_], A] = F[Option[A]]

object OptionT:
  def apply[F[_], A](fa: F[Option[A]]): OptionT[F, A] = fa
  extension [F[_], A](o: OptionT[F, A])
    def value: F[Option[A]] = o

  given optionTMonad[F[_]](using F: Monad[F]): Monad[OptionT[F, _]] with
    def unit[A](a: => A): OptionT[F, A] = F.unit(Some(a))
    extension [A](fa: OptionT[F, A])
      override def flatMap[B](f: A => OptionT[F, B]): OptionT[F, B] =
        F.flatMap(fa) {
          case None => F.unit(None)
          case Some(a) => f(a).value
        }
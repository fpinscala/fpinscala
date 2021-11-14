trait Foldable[F[_]]:
  extension [A](as: F[A])
    def toList: List[A] =
      as.foldRight(List.empty[A])(_ :: _)

object Foldable:
  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def toList: List[A] = as
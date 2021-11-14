given Foldable[Option] with
  extension [A](as: Option[A])
    override def foldRight[B](acc: B)(f: (A, B) => B) = as match
      case None => acc
      case Some(a) => f(a, acc)
    override def foldLeft[B](acc: B)(f: (B, A) => B) = as match
      case None => acc
      case Some(a) => f(acc, a)
    override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      as match
        case None => mb.empty
        case Some(a) => f(a)
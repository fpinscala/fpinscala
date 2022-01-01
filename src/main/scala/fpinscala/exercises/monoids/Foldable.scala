package fpinscala.exercises.monoids

trait Foldable[F[_]]:
  import Monoid.{endoMonoid, dual}

  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      ???

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      ???

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      ???

    def combineAll(using ma: Monoid[A]): A =
      ???

    def toList: List[A] =
      ???

object Foldable:

  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        ???
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        ???
      override def toList: List[A] =
        ???

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        ???
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        ???
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        ???

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        ???
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        ???

  import fpinscala.exercises.datastructures.Tree

  given Foldable[Tree] with
    import Tree.{Leaf, Branch}
    extension [A](as: Tree[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        ???
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        ???
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        ???

  given Foldable[Option] with
    extension [A](as: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        ???
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        ???
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        ???

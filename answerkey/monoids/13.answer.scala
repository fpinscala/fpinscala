import fpinscala.datastructures.Tree

given Foldable[Tree] with
  import Tree.{Leaf, Branch}
  extension [A](as: Tree[A])
    override def foldRight[B](acc: B)(f: (A, B) => B) = as match
      case Leaf(a) => f(a, acc)
      case Branch(l, r) => l.foldRight(r.foldRight(acc)(f))(f)
    override def foldLeft[B](acc: B)(f: (B, A) => B) = as match
      case Leaf(a) => f(acc, a)
      case Branch(l, r) => r.foldLeft(l.foldLeft(acc)(f))(f)
    override def foldMap[B](f: A => B)(using mb: Monoid[B]): B = as match
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.combine(l.foldMap(f), r.foldMap(f))
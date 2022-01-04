given listTraverse: Traverse[List] with
  extension [A](as: List[A])
    override def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
      val g = summon[Applicative[G]]
      as.foldRight(g.unit(List[B]()))((a, acc) => f(a).map2(acc)(_ :: _))

given optionTraverse: Traverse[Option] with
  extension [A](oa: Option[A])
    override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Option[B]] =
      oa match
        case Some(a) => f(a).map(Some(_))
        case None    => summon[Applicative[G]].unit(None)

given treeTraverse: Traverse[Tree] = new:
  extension [A](ta: Tree[A])
    override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Tree[B]] =
      f(ta.head).map2(ta.tail.traverse(a => a.traverse(f)))(Tree(_, _))

given mapTraverse[K]: Traverse[Map[K, _]] with
  extension [A](m: Map[K, A])
    override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Map[K, B]] =
      m.foldLeft(summon[Applicative[G]].unit(Map.empty[K, B])) { case (acc, (k, a)) =>
        acc.map2(f(a))((m, b) => m + (k -> b))
      }

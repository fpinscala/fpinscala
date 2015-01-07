def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f))(_ :: _)
  }

def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
  traverse(a)(x => x)

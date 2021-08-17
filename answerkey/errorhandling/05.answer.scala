def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
  as match
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f))(_ :: _)

def traverse_1[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
  as.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
  traverse(as)(x => x)

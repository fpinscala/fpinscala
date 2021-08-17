def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
  as match
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)

def traverse_1[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
  as.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

def sequence[E,A](as: List[Either[E,A]]): Either[E,List[A]] = 
  traverse(as)(x => x)
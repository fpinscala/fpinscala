/*
Here's an explicit recursive version:
*/
def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }
/*
It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an unfortunate consequence of Scala using subtyping to encode algebraic data types.
*/
def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
  a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))
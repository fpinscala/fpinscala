
/*
For `Par`, `filterM` filters a list, applying the functions in
parallel; for `Option`, it filters a list, but allows
the filtering function to fail and abort the filter
computation; for `Gen`, it produces a generator for 
subsets of the input list, where the function `f` picks a 
'weight' for each element (in the form of a
`Gen[Boolean]`)
*/
def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
  ms match {
    case Nil => unit(Nil)
    case h :: t => flatMap(f(h))(b =>
      if (!b) filterM(t)(f)
      else map(filterM(t)(f))(h :: _))
  }

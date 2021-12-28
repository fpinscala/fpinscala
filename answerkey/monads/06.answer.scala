/*
For `Par`, `filterM` filters a list, applying the functions in
parallel; for `Option`, it filters a list, but allows
the filtering function to fail and abort the filter
computation; for `Gen`, it produces a generator for 
subsets of the input list, where the function `f` picks a 
'weight' for each element (in the form of a
`Gen[Boolean]`)
*/
def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
  as.foldRight(unit(List[A]()))((a, acc) =>
    f(a).flatMap(b => if b then unit(a).map2(acc)(_ :: _) else acc))
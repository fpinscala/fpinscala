/*
`append` simply replaces the `Nil` constructor of the first list with the second list, which is exactly the operation performed by `foldRight`.
*/
def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = 
  foldRight(l, r)(Cons(_,_))
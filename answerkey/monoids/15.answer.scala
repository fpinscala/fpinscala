def toList[A](as: F[A]): List[A] =
  foldRight(as)(List[A]())(_ :: _)
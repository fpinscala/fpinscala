def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = 
  List.fill(n)(g).foldRight(unit(List[A]()))((a,b) => a.map2(b)(_ :: _))
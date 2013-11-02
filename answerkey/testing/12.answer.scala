def listOf[A](g: Gen[A]): SGen[List[A]] = 
  SGen(n => g.listOfN(n))
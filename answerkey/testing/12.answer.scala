extension [A](self: Gen[A]) def list: SGen[List[A]] =
  n => self.listOfN(n)

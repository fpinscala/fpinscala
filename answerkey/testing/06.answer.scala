extension [A](self: Gen[A]) def flatMap[B](f: A => Gen[B]): Gen[B] =
  State.flatMap(self)(f)

extension [A](self: Gen[A]) def listOfN(size: Gen[Int]): Gen[List[A]] =
  size.flatMap(listOfN)

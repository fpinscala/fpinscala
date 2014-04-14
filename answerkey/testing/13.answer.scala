def listOf1[A](g: Gen[A]): SGen[List[A]] =
  SGen(n => g.listOfN(n max 1))
    
val maxProp1 = forAll(listOf1(smallInt)) { l => 
  val max = l.max
  !l.exists(_ > max) // No value greater than `max` should exist in `l`
}
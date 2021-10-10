extension [A](self: Gen[A]) def nonEmptyList: SGen[List[A]] =
  n => listOfN(n.max(1))

val maxProp = Prop.forAll(smallInt.nonEmptyList) { ns => 
  val max = ns.max
  !ns.exists(_ > max)
}

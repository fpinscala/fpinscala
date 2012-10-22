val sortedProp = forAll(listOf(smallInt)) { l => 
  val ls = l.sorted
  l.isEmpty || !l.zip(l.tail).exists { case (a,b) => a > b }
}
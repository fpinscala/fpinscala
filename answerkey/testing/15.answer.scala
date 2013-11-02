// We specify that every sorted list is either empty, has one element,
// or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
val sortedProp = forAll(listOf(smallInt)) { l =>
  val ls = l.sorted
  l.isEmpty || ls.tail.isEmpty || !l.zip(l.tail).exists { case (a,b) => a > b }
}
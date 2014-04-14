val sortedProp = forAll(listOf(smallInt)) { ns =>
  val nss = ns.sorted
  // We specify that every sorted list is either empty, has one element,
  // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
  (ns.isEmpty || nss.tail.isEmpty || !ns.zip(ns.tail).exists {
    case (a,b) => a > b
  })
    // Also, the sorted list should have all the elements of the input list,
    && !ns.exists(!nss.contains(_))
    // and it should have no elements not in the input list.
    && !nss.exists(!ns.contains(_))
}
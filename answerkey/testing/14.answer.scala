val sortedProp = Prop.forAll(smallInt.list) { l =>
  val ls = l.sorted
  val ordered = l.isEmpty || ls.zip(ls.tail).forall { (a, b) => a <= b }
  ordered && l.forall(ls.contains) && ls.forall(l.contains)
}

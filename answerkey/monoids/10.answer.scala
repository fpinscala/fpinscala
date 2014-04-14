sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

val wcMonoid: Monoid[WC] = new Monoid[WC] {
  // The empty result, where we haven't seen any characters yet.
  val zero = Stub("")

  def op(a: WC, b: WC) = (a, b) match {
    case (Stub(c), Stub(d)) => Stub(c + d)
    case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
    case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
    case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
      Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
  }
}
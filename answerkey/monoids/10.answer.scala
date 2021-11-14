enum WC:
  case Stub(chars: String)
  case Part(lStub: String, words: Int, rStub: String)

val wcMonoid: Monoid[WC] = new Monoid[WC]:
  val empty = WC.Stub("")

  def combine(wc1: WC, wc2: WC) = (wc1, wc2) match
    case (WC.Stub(a), WC.Stub(b)) => WC.Stub(a + b)
    case (WC.Stub(a), WC.Part(l, w, r)) => WC.Part(a + l, w, r)
    case (WC.Part(l, w, r), WC.Stub(a)) => WC.Part(l, w, r + a)
    case (WC.Part(l1, w1, r1), WC.Part(l2, w2, r2)) =>
      WC.Part(l1, w1 + (if (r1 + l2).isEmpty then 0 else 1) + w2, r2)
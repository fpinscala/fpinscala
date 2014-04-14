def count(s: String): Int = {
  def wc(c: Char): WC =
    if (c.isWhitespace)
      Part("", 0, "")
    else
      Stub(c.toString)
  def unstub(s: String) = s.length min 1
  foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
    case Stub(s) => unstub(s)
    case Part(l, w, r) => unstub(l) + w + unstub(r)
  }
}
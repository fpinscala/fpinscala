def count(s: String): Int =
  // A single character's count. Whitespace does not count,
  // and non-whitespace starts a new Stub.
  def wc(c: Char): WC =
    if c.isWhitespace then
      WC.Part("", 0, "")
    else
      WC.Stub(c.toString)
  def unstub(s: String) = if s.isEmpty then 0 else 1
  foldMapV(s.toIndexedSeq, wcMonoid)(wc) match
    case WC.Stub(s) => unstub(s)
    case WC.Part(l, w, r) => unstub(l) + w + unstub(r)
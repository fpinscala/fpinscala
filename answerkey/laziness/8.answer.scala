def from(n: Int): Stream[Int] = new Stream[Int] {
  def uncons = Some((n, from(n+1)))
}
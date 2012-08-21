def constant[A](a: A): Stream[A] = new Stream[A] {
  def uncons = Some((a, this))
}
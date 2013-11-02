def from(n: Int): Stream[Int] = new Cons[Int] {
  val head = n
  lazy val tail = from(n+1)
}
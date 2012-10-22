def trimMonoid(s: String): Monoid[String] = new Monoid[String] {
  def op(a: String, b: String) = (a.trim + " " + b.trim).trim
  val zero = ""
}

val intAddition: Monoid[Int] = new Monoid[Int] {
  def op(x: Int, y: Int) = x + y
  val zero = 0
}

val intMultiplication: Monoid[Int] = new Monoid[Int] {
  def op(x: Int, y: Int) = x * y
  val zero = 1
}

val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
  def op(x: Boolean, y: Boolean) = x || y
  val zero = false
}

val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
  def op(x: Boolean, y: Boolean) = x && y
  val zero = true
}
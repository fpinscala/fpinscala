object Absolute1 {
  def absolute(f: Int => Int): Int => Int =
    n => f(n).abs // This uses the built-in `abs` method on `Int`
}
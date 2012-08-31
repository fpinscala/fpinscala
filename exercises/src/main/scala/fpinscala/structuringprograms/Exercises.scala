package fpinscala.structuringprograms

case class Box(height: Double, width: Double)

object Exercises {

  def greaterBy(x: Box, y: Box, f: Box => Double): Box = 
  if (f(x) > f(y)) x else y

  type Pred[A] = A => Boolean

  def absolute(f: Int => Int): Int => Int = sys.error("todo")

  def divisibleBy(k: Int): Pred[Int] = sys.error("todo")

  def lift[A](f: (Boolean, Boolean) => Boolean,
              g: Pred[A],
              h: Pred[A]): Pred[A] = sys.error("todo")

  def curry[A,B,C](f: (A, B) => C): A => B => C = sys.error("todo")

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = sys.error("todo")

  def compose[A,B,C](f: B => C, g: A => B): A => C = sys.error("todo")

  def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B,
                                          h: A => C,
                                          i: A => D): A => E = sys.error("todo")

  def fib(n: Int): Int = sys.error("todo")

  def sqrt(n: Double): Double = {
    def f(x: Double) = (x * x) - n // We want to find the `x` such that `x` squared minus `n` equals `0`.
    iterateWhile(2.0)(x => x - f(x) / (2 * x), // Starting with a guess of `2.0`, iteratively improve the guess.
                      x => f(x).abs > 1e-14) // `1e-14` is a way of writing `10` to the `-14`th power, a rather small number. When the difference between the guess and the answer is smaller than this, the guess is "good enough".
  }
  
  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A = sys.error("todo")
}
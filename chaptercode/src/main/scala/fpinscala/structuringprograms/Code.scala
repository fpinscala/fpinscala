package fpinscala.structuringprograms

object MyModule { 
  def abs(n: Int): Int =
    if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val s = "The absolute value of %d is %d."
    s.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}

case class Box(height: Double, width: Double)

object Examples {
  def greaterBy(x: Box, y: Box, f: Box => Double): Box = 
    if (f(x) > f(y)) x else y
  
  def wider(x: Box, y: Box): Box =
    greaterBy(x, y, p => p.width)
  
  def taller(x: Box, y: Box) =
    greaterBy(x, y, p => p.height)

  def even(n: Int): Boolean =
    n % 2 == 0 // -
  
  def negative(n: Int): Boolean =
    n < 0

  val odd = not(even)
  val positive = not(negative)

  def not[A](p: A => Boolean): A => Boolean =
    a => !p(a)

  type Pred[A] = A => Boolean
}
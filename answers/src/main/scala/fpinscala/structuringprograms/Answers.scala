package fpinscala.structuringprograms

case class Box(height: Double, width: Double)

object Answers {

  def greaterBy(x: Box, y: Box, f: Box => Double): Box = 
  if (f(x) > f(y)) x else y

  type Pred[A] = A => Boolean

  def wider(x: Box, y: Box): Box =
    greaterBy(x, y, _.width)
  
  def taller(x: Box, y: Box) =
    greaterBy(x, y, _.height)

  object Absolute1 {
    def absolute(f: Int => Int): Int => Int =
      n => f(n).abs // This uses the built-in `abs` method on `Int`
  }

  def absolute[A](f: A => Int): A => Int =
    a => f(a).abs

  def divisibleBy(k: Int): Pred[Int] = _ % k == 0

  val even = divisibleBy(2)

  object Predicates {
    val _divisibleBy3And5: Pred[Int] = n => divisibleBy(3)(n) && divisibleBy(5)(n)
    val _divisibleBy3Or5: Pred[Int] = n => divisibleBy(3)(n) || divisibleBy(5)(n)
  
    def lift[A](f: (Boolean, Boolean) => Boolean, g: Pred[A], h: Pred[A]): Pred[A] = 
      a => f(g(a), h(a))
  
    val divisibleBy3And5: Pred[Int] = lift(_ && _, divisibleBy(3), divisibleBy(5))
    val divisibleBy3Or5: Pred[Int]  = lift(_ || _, divisibleBy(3), divisibleBy(5))
  }
  /*
  Calling `divisibleBy(0)` results in an error. But we get different
  results for these two expressions:
  
  lift(_ || _, divisibleBy(2), divisibleBy(0))
  (n: Int) => divisibleBy(2)(n) || divisibleBy(0)(n)
  
  Try them with different inputs. Why do you think one of them fails with
  an error for even numbers and the other one just returns `true` without
  failing? Do you think this has any implications for referential
  transparency? Make a note of your thoughts and revisit this question
  after reading the chapter on "strictness and laziness".
  */

  def curry[A,B,C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)
  
  /* The `Function2` trait has a `curried` method already. */

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)
  
  /*
  There is a method on the `Function` object in the standard library,
  `Function.uncurried` that you can use for uncurrying.
  
  Note that we can go back and forth between the two forms. We can curry and uncurry
  and the two forms are in some sense "the same". In FP jargon, we say that they
  are _isomorphic_ ("iso" = same; "morphe" = shape, form), a term we inherit from
  category theory.
  */

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B, h: A => C, i: A => D): A => E =
    a => f(g(a), h(a), i(a))

  object Lift3ReusingLift {
    def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B, h: A => C, i: A => D): A => E =
      a => lift[A,C,D,E](f(g(a), _, _))(h, i)(a)
  }

  def fibRec(n: Int): Int = if (n < 2) n else fib(n - 1) + fib(n - 2)
  
  // The recursive definition is very slow, and uses binary recursion (calls itself twice).
  // Here is one example of a tail-recursive definition:
  def fib(n: Int): Int = {
    def loop(n: Int, x: Int, y: Int): Int =
      if (n == 0) x else loop(n - 1, y, x + y)
    loop(n, 0, 1)
  }
  
  // 0 and 1 are the first two numbers in the sequence, so we start the accumulators with those.
  // At every iteration, we add the two numbers to get the next one.

  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A =
    if (p(a)) iterateWhile(f(a))(f, p) else a

  def lift[A,B,C,D](f: (B, C) => D)(g: A => B, h: A => C): A => D = a => f(g(a), h(a))
}
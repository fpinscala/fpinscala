package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(x, xs) => xs
    case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, tail) => {
      if (f(x)) dropWhile(tail, f)
      else l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, t) => Cons(x, init(t))
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((a, b) => b + 1)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
  l match {
    case Nil => z
    case Cons(x, xs) => {
      foldLeft(xs, f(z, x))(f)
    }
  }

  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((b, a) => b + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))
  }

  def append2[A](l1: List[A], l2: List[A]): List[A] = {
    foldLeft(l1, l2)((l, v) => Cons(v, l))
  }

  def flatten[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])((a, b) => append(a, b))
  }

  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))
  }

  def dToStr(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))
  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean) = foldRight(l, Nil: List[A])((a, b) => {
    if(f(a)) Cons(a, b) else b
  })

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B])((a, b) => append2(f(a), b))
  }

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => {
    if (f(a)) Cons(a, Nil) else Nil
  })

  def zipWith[A, B, C](l: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, tail1), Cons(y, tail2)) => Cons(f(x, y), zipWith(tail1, tail2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val len = length(sub)

  }
}

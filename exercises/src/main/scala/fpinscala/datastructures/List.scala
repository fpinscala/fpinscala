package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing non-empty lists.

object List {

  // A function that uses pattern matching to add up a list of integers
  def sum(ints: List[Int]): Int = ints match {
    case Nil         ⇒ 0 // The sum of the empty list is 0.
    case Cons(x, xs) ⇒ x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  // A function that uses pattern matching to multiply a list of doubles
  def product(ds: List[Double]): Double = ds match {
    case Nil          ⇒ 1.0 // The product of the empty list is 1.0
    case Cons(0.0, _) ⇒ 0.0 // The product of a list starting with zero is zero.
    case Cons(x, xs)  ⇒ x * product(xs) // The product of a list starting with non-zero `x` is `x` times the product of the rest of the list.
  }

  // A tail-recursive function that creates a `List` of `n` copies of `a`
  def fill[A](n: Int, a: A): List[A] = {
    @annotation.tailrec
    def loop(n: Int, acc: List[A]): List[A] = {
      n match {
        case 0 ⇒ acc
        case _ ⇒ loop(n - 1, Cons(a, acc))
      }
    }

    loop(n, Nil)
  }

  // Exercise 3.2 - Implement the `tail` function for removing the first element of a `List`
  def tail[A](l: List[A]): List[A] = l match {
    case Nil        ⇒ sys.error("tail of empty list") // This is safer than silently returning `Nil`
    case Cons(_, t) ⇒ t
  }

  // Variadic function syntax;
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //  val x = List(1, 2, 3, 4, 5) match {
  //    case Cons(x, Cons(2, Cons(4, _)))          ⇒ x
  //    case Nil                                   ⇒ 42
  //    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) ⇒ x + y
  //    case Cons(h, t)                            ⇒ h + sum(t)
  //    case _                                     ⇒ 101
  //  }
  //
  //  def append[A](a1: List[A], a2: List[A]): List[A] =
  //    a1 match {
  //      case Nil        ⇒ a2
  //      case Cons(h, t) ⇒ Cons(h, append(t, a2))
  //    }
  //
  //  def foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = // Utility functions
  //    as match {
  //      case Nil         ⇒ z
  //      case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f))
  //    }
  //
  //  def sum2(ns: List[Int]) =
  //    foldRight(ns, 0)((x, y) ⇒ x + y)
  //
  //  def product2(ns: List[Double]) =
  //    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
  //
  //  def tail[A](l: List[A]): List[A] = sys.error("todo")
  //
  //  def setHead[A](l: List[A], h: A): List[A] = sys.error("todo")
  //
  //  def drop[A](l: List[A], n: Int): List[A] = sys.error("todo")
  //
  //  def dropWhile[A](l: List[A], f: A ⇒ Boolean): List[A] = sys.error("todo")
  //
  //  def init[A](l: List[A]): List[A] = sys.error("todo")
  //
  //  def length[A](l: List[A]): Int = sys.error("todo")
  //
  //  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) ⇒ B): B = sys.error("todo")
  //
  //  def map[A, B](l: List[A])(f: A ⇒ B): List[B] = sys.error("todo")
}

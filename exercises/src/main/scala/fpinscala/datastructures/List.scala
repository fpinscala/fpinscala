package fpinscala.datastructures

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil         ⇒ 0
    case Cons(x, xs) ⇒ x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          ⇒ 1.0
    case Cons(0.0, _) ⇒ 0.0
    case Cons(x, xs)  ⇒ x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1, 2, 3)
  val total = sum(example)

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          ⇒ x
    case Nil                                   ⇒ 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) ⇒ x + y
    case Cons(h, t)                            ⇒ h + sum(t)
    case _                                     ⇒ 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        ⇒ a2
      case Cons(h, t) ⇒ Cons(h, append(t, a2))
    }

  def concat[A](l: List[List[A]]): List[A] = sys.error("todo") // ex 16

  def add1(l: List[Int]): List[Int] = sys.error("todo") // ex 17

  def doubleToString(l: List[Double]): List[String] = sys.error("todo") // ex 18

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) ⇒ B): B = // Utility functions
    l match {
      case Nil         ⇒ z
      case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = sys.error("todo")

  def drop[A](l: List[A], n: Int): List[A] = sys.error("todo")

  def dropWhile[A](l: List[A])(f: A ⇒ Boolean): List[A] = sys.error("todo")

  def setHead[A](l: List[A])(h: A): List[A] = sys.error("todo")

  def init[A](l: List[A]): List[A] = sys.error("todo")

  def length[A](l: List[A]): Int = sys.error("todo")

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) ⇒ B): B = sys.error("todo")

  def map[A, B](l: List[A])(f: A ⇒ B): List[B] = sys.error("todo")

  def filter[A](l: List[A])(f: A ⇒ Boolean): List[A] =
    /* ex 20 */ sys.error("todo")

  def flatMap[A, B](l: List[A])(f: A ⇒ List[B]): List[B] =
    /* ex 21 */ sys.error("todo")

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    /* ex 23 */ sys.error("todo")

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) ⇒ C): List[C] =
    /* ex 24 */ sys.error("todo")

  def hasSubsequence[A](list: List[A], sub: List[A]): Boolean =
    /* ex 25 */ sys.error("todo")
}

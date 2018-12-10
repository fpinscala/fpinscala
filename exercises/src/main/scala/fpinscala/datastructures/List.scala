package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,Nil) => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else Cons(h, t)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // using foldRight
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // using foldLeft
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  // using foldLeft
  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  // using foldLeft
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => append(reverse(t), Cons(h, Nil))
  }

  // using foldLeft or foldRight
  def reverse2[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  // using foldRight
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((h, acc) => Cons(h, acc))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])((h, acc) => append(h, acc))

  def add1(l: List[Int]): List[Int] = foldLeft(l, Nil: List[Int])((z, h) => append(z, Cons(h+1, Nil)))

  def doubleToStr(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, acc) => append(acc, Cons(h.toString, Nil)))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, acc) => append(acc, Cons(f(h), Nil)))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(as, Nil: List[A])((acc, h) => { if (f(h)) append(acc, Cons(h, Nil)) else acc })

  // IMPORTANT NOTE about odd check at https://nrinaudo.github.io/talk-scala-best-practices/#6
  // example to remove all odd ones, i.e. filter even ones
  val filtered = filter(Cons(1, Cons(2, Cons(3, Nil))))(_ % 2 == 0)

  def flatmap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((h, acc) => append(f(h), acc))

  val flatten = flatmap(List(1,2,3))(i => List(i, i))

  def filterViaFlatmap[A](as: List[A])(f: A => Boolean): List[A] = flatmap(as)(a => if (f(a)) Cons(a, Nil) else Nil )

  def addCorrespondents(a1: List[Int], a2: List[Int]): List[Int] =
    (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addCorrespondents(t1, t2))
    }

  def zipWith[A](a: List[A], b: List[A])(f: (A,A) => A): List[A] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  def take[A](l: List[A], n: Int): List[A] = reverse(drop(reverse(l), length(l)-n))

  @tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    val len = length(l)
    val subLen = length(sub)
    if (len < subLen) false
    else if (len == subLen) if (l == sub) true else false
    else
     if (take(l, subLen) == sub) true
     else hasSubsequence(tail(l), sub)
  }
}

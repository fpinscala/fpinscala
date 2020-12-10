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
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Error("the is no tail in empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new Error("sethead of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(tail(t), n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else Cons(h, t)
  }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new Error("no init for Nil")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((z, h) => Cons(h, z))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((h, z) => f(z, h))

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((z, h) => f(h, z))

  def append2[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append2)

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, z) => Cons(f(h), z))

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, z) => Cons(h + 1, z))

  def double2string(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, z) => Cons(h.toString, z))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil: List[A])((h, z) => if (f(h)) Cons(h, z) else z)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((h, z) => append2(f(h), z))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if (f(x)) List(x) else Nil)

  def zipInts(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Nil, Nil) => Nil
    case (Nil, Cons(_, _)) => throw new Error("length of l != length of r")
    case (Cons(_, _), Nil) => throw new Error("length of l != length of r")
    case (Cons(hl, tl), Cons(hr, tr)) => Cons(hl + hr, zipInts(tl, tr))
  }

  def zipWith[A, B](l: List[A], r: List[A])(f: (A, A) => B): List[B] = (l, r) match {
    case (Nil, Nil) => Nil
    case (Nil, Cons(_, _)) => throw new Error("length of l != length of r")
    case (Cons(_, _), Nil) => throw new Error("length of l != length of r")
    case (Cons(hl, tl), Cons(hr, tr)) => Cons(f(hl, hr), zipWith(tl, tr)(f))
  }

  def take[A](l: List[A], n: Int): List[A] =
    if (n <= 0) Nil
    else l match {
      case Nil => throw new Error("n more than list length")
      case Cons(h, t) => Cons(h, take(t, n - 1))
    }

  @tailrec
  def beginsWith[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(hl, tl), Cons(hsub, tsub)) => if (hl != hsub) false else beginsWith(tl, tsub)
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case Cons(h, t) => if (beginsWith(Cons(h, t), sub)) true else hasSubsequence(t, sub)
  }

  def main(args: Array[String]): Unit = {
    val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val l2 = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Nil))))

    println(tail(l))
    println(setHead(l, 666))
    println(drop(l, 2))
    println(dropWhile[Int](l, _ < 4))
    println(init(l))
    println(length(l))
    println(sum3(l))
    println(product3(l2))
    println(reverse(l))

    println()
    println(foldRight(l, 0)((h, z) => {
      println(s"z - $z, h - $h")
      z + h
    }))

    println()
    println(foldRight2(l, 0)((h, z) => {
      println(s"z - $z, h - $h")
      z + h
    }))

    println()
    println(foldLeft(l, 0)((z, h) => {
      println(s"z - $z, h - $h")
      z + h
    }))
    println()
    println(foldLeft2(l, 0)((z, h) => {
      println(s"z - $z, h - $h")
      z + h
    }))
    println()

    println(append(l, l2))


    val f1 = Cons(1, Cons(2, Nil))
    val f2 = Cons(3, Cons(4, Nil))
    println(flatten(Cons(f1, Cons(f2, Nil))))

    println(add1(l))
    println(double2string(l2))
    println(map[Int, Int](l)(_ * 2))
    println(filter(l)(_ % 2 == 0))
    println(flatMap(l)(i => List(i, i)))
    println(filter2(l)(_ % 2 == 0))

    println(zipInts(List(1, 2, 3), List(4, 5, 6)))
    println(zipWith(List(1, 2, 3), List(4, 5, 6))(_ * _))

    println()
    println(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    println(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    println(hasSubsequence(List(1, 2, 3, 4), List(3, 4, 5)))
    println(hasSubsequence(List(1, 2, 3, 4), List(0, 1, 2)))
    println(hasSubsequence(List(), List()))
  }
}

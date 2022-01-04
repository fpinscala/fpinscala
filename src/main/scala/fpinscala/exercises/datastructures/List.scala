package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val x = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = ???

  def setHead[A](l: List[A], h: A): List[A] = ???

  def drop[A](l: List[A], n: Int): List[A] = ???

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def init[A](l: List[A]): List[A] = ???

  def length[A](l: List[A]): Int = ???

  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = ???

  def sumViaFoldLeft(ns: List[Int]) = ???

  def productViaFoldLeft(ns: List[Double]) = ???

  def lengthViaFoldLeft[A](l: List[A]): Int = ???

  def reverse[A](l: List[A]): List[A] = ???

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = ???

  def concat[A](l: List[List[A]]): List[A] = ???

  def incrementEach(l: List[Int]): List[Int] = ???

  def doubleToString(l: List[Double]): List[String] = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  def filter[A](as: List[A])(f: A => Boolean): List[A] = ???

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = ???

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = ???

  // def zipWith - TODO determine signature

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???

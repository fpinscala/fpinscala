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


  // Exercise 3.2: Implement a function for removing the first element of a `List`
  def tail[A](l: List[A]): List[A] = ???

  // Exercise 3.3: Implement a function for replacing the first element of a `List` with a different value
  def setHead[A](l: List[A], h: A): List[A] = ???

  // Exercise 3.4: Generalise `tail` to a function which removes the first `n` elements from a list
  def drop[A](l: List[A], n: Int): List[A] = ???

  // Exercise 3.5: Implement a function which removes elements from the `List` prefix as long as they match a predicate
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  // Exercise 3.6: Implement a function that returns a `List` consisting of all but the last element of a `List`
  def init[A](l: List[A]): List[A] = ???

  // Exercise 3.9: Compute the length of a list using `foldRight`
  def length[A](l: List[A]): Int = ???

  // Exercise 3.10: Write a general list-recursive function that is tail-recursive
  // `l` is the list, `z` is initial value, `f` is the combining operation
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  // Exercise 3.11: Write `sum`, `product`, and a function to compute the length of a list using `foldLeft`

  // Exercise 3.12: Write a function that returns the reverse of a list

  // Exercise 3.13: Write `foldLeft` in terms of `foldRight`

  // Exercise 3.14: Implement `append` in terms of either `foldLeft` or `foldRight`

  // Exercise 3.15: Write a function that concatenates a list of lists into a single list

  // Exercise 3.16: Write a function that transforms a list of integers by adding 1 to each element

  // Exercise 3.17: Write a function that turns each value in a `List[Double]` into a `String`

  // Exercise 3.18: Write a function `map` that generalizes modifying each element in a list while maintaining the
  // structure of the list
  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  // Exercise 3.19: Write a function `filter` that removes elements from a list unless they satisfy a given predicate

  // Exercise 3.20: Write a function `flatMap` that works like `map` except that the function given will return a list
  // instead of a single result

  // Exercise 3.21: Use `flatMap` to implement `filter`

  // Exercise 3.22: Write a function that accepts two lists and constructs a new list by adding corresponding elements

  // Exercise 3.23: Generalise the function you just wrote so that it's not specific to integers or addition

  // Exercise 3.24: Implement `hasSubsequence` for checking whether a `List` contains another `List` as a subsequence
}

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
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead of empty list")
    case Cons(_, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) dropWhile(tail, f) else l
  }

  def addToList[A](l: List[A], elementToAdd: A): List[A] = l match {
    case Nil => Cons(elementToAdd, Nil)
    case Cons(head, tail) => Cons(head, addToList(tail, elementToAdd))
  }

  def init[A](l: List[A]): List[A] = {

    def loop(list: List[A], resultList: List[A], lastHead: A): List[A] = list match {
      case Nil => resultList
      case Cons(head, tail) => loop(tail, addToList(resultList, lastHead), head)
    }

    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => loop(tail, Nil, head)
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => 1 + y)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, elem) => Cons(elem, acc))

  /*
    reverse(Cons(1, Cons(2, Cons(3, Nil))))
    foldLeft(Cons(1, Cons(2, Cons(3, Nil))), Nil)((acc, elem) => Cons(elem, acc))
    foldLeft(Cons(2, Cons(3, Nil)), Cons(1, Nil))((acc, elem) => Cons(elem, acc))
    foldLeft(Cons(3, Nil), Cons(2, Cons(1, Nil)))((acc, elem) => Cons(elem, acc))
    foldLeft(Nil, Cons(3, Cons(2, Cons(1, Nil))))((acc, elem) => Cons(elem, acc))
    Cons(3, Cons(2, Cons(1, Nil)))
   */

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((elem, acc) => Cons(elem, acc))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(append)

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((elem, acc) => Cons(elem + 1, acc))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => append(f(h), t))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(elem => if (f(elem)) List(elem) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  def zipWith[A, B](a: List[A], b: List[A])(f: (A, A) => B): List[B] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(h1, t1), Cons(h2, t2)) =>
      if (h1 == h2) hasSubsequence(t1, t2) else hasSubsequence(t1, sub)
  }
}

object run {
  def main(args: Array[String]): Unit = {
    import List._
    val emptyList = Nil
    val stringList = List("zero", "one", "two", "three", "four")
    val intList = List(0, 1, 2, 3, 4)
    println(s"Dropping on string list: ${drop(stringList, 2)}")
    println(s"Dropping on empty list: ${drop(emptyList, 1)}")
    println(s"DropWhile on Int list: ${dropWhile[Int](intList, x => x < 2)}")
    println(s"Init on stringList list: ${init(stringList)}")
    println(s"Add to list: ${addToList(stringList, "addedElement")}")
    println(s"Length of stringList: ${length(stringList)}")

    val sub1 = List(0, 1, 2, 3, 4)
    val sub2 = List(1, 2, 3)
    val sub3 = List(2,3)
    val sub4 = List(4, 5)
    val sup1 = List(0, 0, 1, 4)
    val sub5 = List(0, 1, 4)
    val sup2 = append(append(sub1, sub4), sub5)
    println(s"HasSubsequence($intList, $sub1) should be true: ${hasSubsequence(intList, sub1)}")
    println(s"HasSubsequence($intList, $sub2) should be true: ${hasSubsequence(intList, sub2)}")
    println(s"HasSubsequence($intList, $sub3) should be true: ${hasSubsequence(intList, sub3)}")
    println(s"HasSubsequence($intList, $sub4) should be false: ${hasSubsequence(intList, sub4)}")
    println(s"HasSubsequence($sup1, $sub5) should be true: ${hasSubsequence(sup1, sub5)}")
    println(s"HasSubsequence($sup2, $sub4) should be true: ${hasSubsequence(sup2, sub4)}")
  }
}

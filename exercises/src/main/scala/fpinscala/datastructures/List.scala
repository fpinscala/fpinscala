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

  def length[A](l: List[A]): Int = ???

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A, B](l: List[A])(f: A => B): List[B] = ???
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
  }
}

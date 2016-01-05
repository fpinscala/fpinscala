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


  def tail[A](l: List[A]): List[A] = sys.error("todo")

  def setHead[A](l: List[A], h: A): List[A] = sys.error("todo")

  def drop[A](l: List[A], n: Int): List[A] = sys.error("todo")

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = sys.error("todo")

  def init[A](l: List[A]): List[A] = sys.error("todo")

  def length[A](l: List[A]): Int = sys.error("todo")

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")

  def test_sum(sum: List[Int] => Int): Unit =
  {
    assert( sum(           Nil ) ==  0, "sum of empty list should be 0")
    assert( sum(       List(5) ) ==  5, "sum of single-element list should be the element" )
    assert( sum( List(1,2,3,4) ) == 10, "sum of list should be sum of its elements" )
  }

  def test_product(product: List[Double] => Double): Unit =
  {
    assert( product( Nil)                       ==  1.0,  "product of empty list should be 1.0" )
    assert( product( List(7.0))                 ==  7.0,  "product of single-element list should be the element" )
    assert( product( List(1.0, 2.0, 3.0, 4.0) ) == 24.0,  "product of list should be product of its elements" )
    assert( product( List(1.0, 2.0, 0.0, 4.0) ) ==  0.0,  "product of list containing zero should be zero" )
  }

  def test_append(): Unit =
  {
    assert( append( Nil,             Nil ) ==           Nil, "append of two empty lists should be empty list")
    assert( append( Nil,         List(3) ) ==       List(3), "append of empty list to a list should be list")
    assert( append( List(3),         Nil ) ==       List(3), "append of list to empty list should be list")
    assert( append( List(1,2),   List(3) ) ==   List(1,2,3), "append of list to one-element list should be concatenation of lists")
    assert( append( List(1),   List(2,3) ) ==   List(1,2,3), "append of one-element list to list should be concatenation of lists")
    assert( append( List(1,2), List(3,4) ) == List(1,2,3,4), "append of two lists should be concatenation of lists")
  }

  def test_tail(): Unit =
  {
    assert( tail(         Nil ) ==       Nil, "tail of Nil should be Nil")
    assert( tail(     List(3) ) ==       Nil, "tail of single-element list should be Nil")
    assert( tail( List(1,2,3) ) == List(2,3), "tail of list should be rest")
  }

  def test_setHead(): Unit =
  {
    assert( setHead(       Nil, 1 ) ==       Nil, "setHead of empty list should be empty list")
    assert( setHead(   List(2), 1 ) ==   List(1), "setHead of single-element list should be two-element list")
    assert( setHead( List(3,2), 1 ) == List(1,2), "setHead of two-element list should be three-element list")
  }

  def test_drop(): Unit =
  {
    assert( drop( Nil,          0) ==         Nil, "drop of zero elements from empty list is empty list")
    assert( drop( Nil,          1) ==         Nil, "drop of one element from empty list is empty list")
    assert( drop( Nil,         10) ==         Nil, "drop of many elements from empty list is empty list")
    assert( drop( List(3),      0) ==     List(3), "drop of zero elements from single-element list is the list")
    assert( drop( List(3),      1) ==         Nil, "drop of one element from single-element list is empty list")
    assert( drop( List(3),     10) ==         Nil, "drop of many elements from single-element list is empty list")
    assert( drop( List(1,2,3),  0) == List(1,2,3), "drop of zero elements from list is list")
    assert( drop( List(1,2,3),  1) ==   List(2,3), "drop of one elements from list is list without 1st element")
    assert( drop( List(1,2,3),  2) ==     List(3), "drop of n elements from list is list without 1st n elements")
    assert( drop( List(1,2,3), 10) ==         Nil, "drop of too many elements from list is empty list")
  }

  def test_dropWhile(): Unit =
  {
    val positive = (x: Int) => x > 0
    assert( dropWhile(                  Nil, positive ) ==                  Nil, "dropWhile of empty list should be empty list")
    assert( dropWhile(              List(1), positive ) ==                  Nil, "dropWhile of list with single valid element should be empty list")
    assert( dropWhile( List( 1,  2,  3,  4), positive ) ==                  Nil, "dropWhile of list with only valid elements should be empty list")
    assert( dropWhile( List( 1,  2, -3,  4), positive ) ==          List(-3, 4), "dropWhile of list with two leading valid elements should be list without leading elements")
    assert( dropWhile( List( 1, -2, -3,  4), positive ) ==      List(-2, -3, 4), "dropWhile of list with one leading valid element should be list without leading element")
    assert( dropWhile( List(-1, -2, -3,  4), positive ) ==  List(-1, -2, -3, 4), "dropWhile of list with no leading valid elements should be same list")
    assert( dropWhile( List(-1, -2, -3, -4), positive ) == List(-1, -2, -3, -4), "dropWhile of list with no valid elements should be Nil")
  }

  def test_init(): Unit =
  {
    assert( init(         Nil ) ==       Nil, "init of empty list should be empty list")
    assert( init(     List(3) ) ==       Nil, "init of single-element-list should be empty list")
    assert( init( List(1,2,3) ) == List(1,2), "init of list should not have last element")
  }

  def test_length(): Unit =
  {
    assert( length(         Nil ) == 0, "length of empty list is zero")
    assert( length(     List(1) ) == 1, "length of single-element list is one")
    assert( length( List(1,2,3) ) == 3, "length of n-element list is n")
  }

  def test_foldLeft(): Unit =
  {
    assert( foldLeft(  List(1, 2, 3, 4, 5), 0) (_ + _) ==
      foldRight( List(1, 2, 3, 4, 5), 0) (_ + _),
      "foldLeft should compute the same sum value as foldRight")

    assert( foldLeft(  List(1, 2, 3, 4, 5), 1) (_ * _) ==
      foldRight( List(1, 2, 3, 4, 5), 1) (_ * _),
      "foldLeft should compute the same product value as foldRight")
  }

  def test(): Unit = {
    test_sum(sum)
    test_sum(sum2)
    test_product(product)
    test_product(product2)
    test_append
    test_tail
    test_setHead
    test_drop
    test_dropWhile
    test_init
    test_length
    test_foldLeft
  }
}

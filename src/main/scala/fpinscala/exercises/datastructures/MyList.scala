package fpinscala.exercises.datastructures

/** List data type, parameterized on a type, `A`. */
enum MyList[+A]:
  /** A `MyList` data constructor representing the empty list. */
  case MyNil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `MyList[A]`,
    which may be `MyNil` or another `Cons`.
   */
  case Cons(head: A, tail: MyList[A])

object MyList: // `MyList` companion object. Contains functions for creating and working with lists.
  def sum(ints: MyList[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case MyNil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: MyList[Double]): Double = ds match
    case MyNil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): MyList[A] = // Variadic function syntax
    if as.isEmpty then MyNil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val x = MyList(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case MyNil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match
      case MyNil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: MyList[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case MyNil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: MyList[Int]) =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: MyList[Double]) =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: MyList[A]): MyList[A] = ???

  def setHead[A](l: MyList[A], h: A): MyList[A] = ???

  def drop[A](l: MyList[A], n: Int): MyList[A] = ???

  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = ???

  def init[A](l: MyList[A]): MyList[A] = ???

  def length[A](l: MyList[A]): Int = ???

  def foldLeft[A,B](l: MyList[A], acc: B, f: (B, A) => B): B = ???

  def sumViaFoldLeft(ns: MyList[Int]) = ???

  def productViaFoldLeft(ns: MyList[Double]) = ???

  def lengthViaFoldLeft[A](l: MyList[A]): Int = ???

  def reverse[A](l: MyList[A]): MyList[A] = ???

  def appendViaFoldRight[A](l: MyList[A], r: MyList[A]): MyList[A] = ???

  def concat[A](l: MyList[MyList[A]]): MyList[A] = ???

  def incrementEach(l: MyList[Int]): MyList[Int] = ???

  def doubleToString(l: MyList[Double]): MyList[String] = ???

  def map[A,B](l: MyList[A])(f: A => B): MyList[B] = ???

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = ???

  def flatMap[A,B](as: MyList[A])(f: A => MyList[B]): MyList[B] = ???

  def addPairwise(a: MyList[Int], b: MyList[Int]): MyList[Int] = ???

  // def zipWith - TODO determine signature

  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = ???

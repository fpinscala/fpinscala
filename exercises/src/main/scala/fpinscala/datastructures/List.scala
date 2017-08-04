package fpinscala.datastructures


/**
  * The makeup of an _Algebraic Data Type_ (ADT) is generally as follows:
  *
  *   - a sealed trait representing the data type
  *   - objects and/or classes to extend the sealed trait
  *
  * In the example below, we implement a singly-linked list as an ADT. Linked lists are exhaustively represented as
  * a singleton `Nil` representing the empty list, and a `Cons` representing a non-empty list. Note the use of a case
  * object for the empty list, and a case class for the nonempty list.
  *
  * Note also that our implementation is polymorphic in the type of elements on the list. The `+` sign on the type
  * variable indicates that this implementation is _covariant_ in its type argument. This means that, for all pairs of
  * types `T1` and `T2`, if `T1` is a subtype of `T2`, then `List[T1]` is a subtype of `List[T2]`.
  *
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// scalastyle:off
object List {

  /**
    * A function that uses pattern matching to add up a list of integers.
    *
    * The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    */
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  // Variadic function syntax
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  /**
    * Exercise 3.1 - What will be the result of the following match expression?
    */
  val x: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // this branch will be triggered, returning `1 + 2 = 3`
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  /**
    * Exercise 3.2 - Implement the function tail for removing the first element of a List.
    *
    * Note that this implementation throws an error if called on an empty `List`.
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil        => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  /**
    * Exercise 3.3 - Using the same idea, implement the function setHead for replacing the first element of a List with
    * a different value.
    *
    * Note that this implementation throws an error if called on an empty `List`.
    */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil        => sys.error("setHead of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  /**
    * Exercise 3.4 - Generalize tail to the function drop, which removes the first n elements from a list.
    *
    * Note that this implementation matches against a `Tuple` of our input parameters.
    */
  def drop[A](l: List[A], n: Int): List[A] = (n, l) match {
    case (k, _) if k <= 0 => l
    case (_, Nil)         => Nil
    case (_, Cons(_, t))  => drop(t, n - 1)
  }

  /**
    * Exercise 3.5 - Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _                  => l
  }

  /**
    * Exercise 3.6 - Implement a function, init, that returns a List consisting of all but the last element of a List.
    *
    * Note that we raise an error if the list is empty. Otherwise, we return `Nil` for a terminal element, and
    * recursively call `init` on the tail of nonempty lists.
    *
    * This function cannot be implemented in constant time since we need to traverse the entire list in order to yield
    * an output. We also copy the list over, element by element, which is not stack-safe.
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil          => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  /**
    * Stack-safe version of `init` above.
    *
    * Note the use of a mutable buffer to accumulate results. This does not break RT, since the mutable
    * data structure is internal to the function and not accessible outside of it.
    */
  def init2[A](l: List[A]): List[A] = {
    val buffer = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def loop(as: List[A]): List[A] = as match {
      case Nil          => sys.error("init2 of empty list")
      case Cons(_, Nil) => List(buffer.toList: _*)
      case Cons(h, t)   => buffer += h; loop(t)
    }

    loop(l)
  }

  /**
    * Exercise 3.7 - Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it
    * encounters a 0.0? Why or why not? Consider how any short-circuiting might work if you call foldRight with a large
    * list. This is a deeper question that we’ll return to in chapter 5.
    *
    * No, because `foldRight` is right-associative and must traverse the entire input `List` before evaluating its input
    * function `f`. Therefore short-circuiting is not possible.
    */

  /**
    * Exercise 3.8 - See what happens when you pass Nil and Cons themselves to foldRight, like this:
    * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).10 What do you think this says about the relationship between
    * foldRight and the data constructors of List?
    *
    * We get the original input `List` back as output. This means that, in addition to `Nil` being our `zero` element
    * and `Cons` being our function to apply, both `foldRight` and our implementation of `List` are right-associative.
    */

  /**
    * Exercise 3.9 - Compute the length of a list using foldRight.
    */
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, x) => x + 1)

  /**
    * Exercise 3.10 - Write another general list-recursion function, foldLeft, that is tail-recursive, using the
    * techniques we discussed in the previous chapter.
    */
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /**
    * Exercise 3.11 - Write sum, product, and a function to compute the length of a list using foldLeft.
    */

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((x, _) => x + 1)

  /**
    * Exercise 3.12 - Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
    * See if you can write it using a fold.
    *
    * Note that this works because `foldLeft` is left-associative and applies to its arguments in reverse order. We
    * already know from exercise 3.8 above that using `Nil` and `Cons` as the arguments to a fold will yield a `List` as
    * the return type. While `foldRight` yields our original input list, using `foldLeft` returns our input list in
    * reverse order.
    */
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc, a) => Cons(a, acc))

  /**
    * Exercise 3.13 - Can you write foldLeft in terms of foldRight? How about the other way around?
    *
    * To illustrate the differences between `foldRight` and `foldLeft` let's follow an example derivation for
    * constructing a `List`, where our "zero" is an empty list and our builder function is `Cons`, as follows:
    *
    * foldRight(List(1, 2, 3), Nil)((a, acc) => Cons(a, acc))
    *
    * becomes:
    * Cons(1, foldRight(List(2, 3), Nil: List[Int])((a, acc) => Cons(a, acc)))
    *
    * becomes:
    * Cons(1, Cons(2, foldRight(List(3), Nil: List[Int])((a, acc) => Cons(a, acc))))
    *
    * becomes:
    * Cons(1, Cons(2, Cons(3, foldRight(List(), Nil: List[Int])((a, acc) => Cons(a, acc)))))
    *
    * becomes:
    * Cons(1, Cons(2, Cons(3, Nil)))
    *
    * and
    *
    * foldLeft(List(1, 2, 3), Nil)((acc, a) => Cons(a, acc))
    *
    * becomes:
    * foldLeft(List(2, 3), Cons(1, Nil))((acc, a) => Cons(a, acc))
    *
    * becomes:
    * foldLeft(List(3), Cons(2, Cons(1, Nil)))((acc, a) => Cons(a, acc))
    *
    * becomes:
    * foldLeft(List(), Cons(3, Cons(2, Cons(1, Nil))))((acc, a) => Cons(a, acc))
    *
    * becomes:
    * Cons(3, Cons(2, Cons(1, Nil)))
    *
    * The thing to note here is that `foldLeft` processes its items in the reverse order from `foldRight`. In order to
    * mimic this, we wrap each operation in an identity function to delay evaluation until later, and stack the
    * functions so that the order of operation can be reversed.
    */
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    // Shortcut for the type `B => B`
    type BtoB = B => B

    /*
     * Here we declare an identity function that we'll use to wrap each operation. Note that the type annotation on the
     * argument `b` is provided for clarity.
     */
    def identity: BtoB = (b: B) => b

    /*
     * Here we augment our combiner function `f` above with a delayer function to postpone evaluation. Note that type
     * annotations on `g` and `a` are given for clarity only
     */
    def combinerDelayer: (BtoB, A) => BtoB = (delay: BtoB, a: A) => (b: B) => delay(f(a, b))

    /*
     * Pass the original `as` to `foldLeft` and use our modified identity and combinerDelayer functions to delay
     * evaluation as needed
     */
    def go: BtoB = foldLeft(as, identity)(combinerDelayer)

    // Trigger application of the delayed function
    go(z)
  }

  /**
    * Derivation of the case above looks like:
    *
    * foldRightViaFoldLeft(List(1, 2, 3), Nil)((a, acc) => Cons(a, acc))
    *
    * becomes:
    * foldLeft(List(1, 2, 3), identity)(combinerDelayer)(Nil)
    *
    * becomes:
    * val delay3 = (b: B) => identity(Cons(1, b))
    * def delay2 = (b: B) => delay3(Cons(2, b))
    * def delay1 = (b: B) => delay2(Cons(3, b))
    * delay1(Nil)
    *
    * becomes:
    * def delay3 = (b: B) => identity(Cons(1, b))
    * def delay2 = (b: B) => delay3(Cons(2, b))
    * delay2(Cons(3, Nil))
    *
    * becomes:
    * def delay3 = (b: B) => identity(combiner(1, b))
    * delay3(Cons(2, Cons(3, Nil)))
    *
    * becomes:
    * identity(Cons(1, Cons(2, Cons(3, Nil))))
    *
    * becomes:
    * Cons(1, Cons(2, Cons(3, Nil)))
    */

  /**
    * Implementation of `foldLeft` via `foldRight` analogous to above.
    *
    * Note the use of `identity` and anonymous functions. While this is much more difficult to understand than the above
    * example, it does the same thing and is much more concise.
    */
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, identity: B => B)((a, g) => b => g(f(b, a)))(z)


  /**
    * Exercise 3.14 - Implement append in terms of either foldLeft or foldRight.
    */
  def append2[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def append3[A](l: List[A], r: List[A]): List[A] =
    foldLeft(reverse(l), r)((acc, a) => Cons(a, acc))

  /**
    * Exercise 3.15 - Hard: Write a function that concatenates a list of lists into a single list. Its runtime should be
    * linear in the total length of all lists. Try to use functions we have already defined.
    */
  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(append2)

  /**
    * Exercise 3.16 - Write a function that transforms a list of integers by adding 1 to each element.
    */
  def addOne(is: List[Int]): List[Int] =
    foldRight(is, Nil: List[Int])((i, acc) => Cons(i + 1, acc))

  /**
    * Exercise 3.17 - Write a function that turns each value in a List[Double] into a String. You can use the expression
    * d.toString to convert some d: Double to a String.
    */
  def doublesToStrings(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((d, acc) => Cons(d.toString, acc))


  /**
    * Exercise 3.18 - Write a function map that generalizes modifying each element in a list while maintain- ing the
    * structure of the list.
    *
    * Note that this implementation uses `foldRight` and is not stack-safe.
    */
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, acc) => Cons(f(a), acc))

  /**
    * Stack-safe version of `map` which uses `foldRightViaFoldLeft`.
    */
  def map2[A, B](as: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(as, Nil: List[B])((a, acc) => Cons(f(a), acc))

  /**
    * Tail-recursive version of `map` which uses a mutable internal structure.
    */
  def map3[A, B](as: List[A])(f: A => B): List[B] = {
    val buffer = new collection.mutable.ListBuffer[B]

    @annotation.tailrec
    def loop(l: List[A]): List[B] = l match {
      case Nil        => List(buffer.toList: _*)
      case Cons(h, t) => buffer += f(h); loop(t)
    }

    loop(as)
  }

  /**
    * Exercise 3.19 - Write a function filter that removes elements from a list unless they satisfy a given predicate.
    *
    * Note that this implementation uses `foldRight` and is not stack-safe, similarly to our implementation of `map`
    * above. The same discussion holds here of `filter`, and it can be implemented in the same alternative ways as we
    * show for `map` above.
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  /**
    * Exercise 3.20 - Write a function flatMap that works like map except that the function given will return a list
    * instead of a single result, and that list should be inserted into the final resulting list.
    *
    * Note that this implementation traverses the elements of the input list twice.
    */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map2(as)(f))

  /**
    * Exercise 3.21 - Use flatMap to implement filter.
    */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  /**
    * Exercise 3.22 - Write a function that accepts two lists and constructs a new list by adding correspond- ing
    * elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    *
    * Note that this implementation is not stack-safe; the same discussion about `map` applies here, and stack-safe
    * versions of this method may be created in similar ways.
    */
  def addPairs(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (_, Nil)                     => Nil
    case (Nil, _)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairs(t1, t2))
  }

  /**
    * Exercise 3.23 - Generalize the function you just wrote so that it’s not specific to integers or addition. Name
    * your generalized function zipWith.
    *
    * Note that this implementation is stack-safe, and uses a tail-recursive inner loop with a mutable buffer.
    */
  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = {
    val buffer = new collection.mutable.ListBuffer[C]

    @annotation.tailrec
    def loop(as: List[A], bs: List[B]): List[C] = (as, bs) match {
      case (_, Nil)                     => List(buffer.toList: _*)
      case (Nil, _)                     => List(buffer.toList: _*)
      case (Cons(h1, t1), Cons(h2, t2)) => buffer += f(h1, h2); loop(t1, t2)
    }

    loop(l, r)
  }

  /**
    * Exercise 3.24 - implement hasSubsequence for checking whether a List con- tains another List as a subsequence. For
    * instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others. You may have
    * some difficulty finding a concise purely functional implementation that is also effi- cient. That’s okay.
    * Implement the function however comes most naturally.
    */
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    // First, define a helper function that tests whether a list contains another list as a prefix
    @annotation.tailrec
    def startsWith[T](l: List[T], prefix: List[T]): Boolean = (l, prefix) match {
      case (_, Nil)                                 => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _                                        => false
    }

    // Now, use our helper method
    sup match {
      case Nil                       => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t)                => hasSubsequence(t, sub)
    }
  }
}
// scalastyle:on

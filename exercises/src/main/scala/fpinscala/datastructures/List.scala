package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing non-empty lists.

object List {

  // A function that uses pattern matching to add up a list of integers
  def sum(ints: List[Int]): Int = ints match {
    case Nil         ⇒ 0
    case Cons(x, xs) ⇒ x + sum(xs)
  }

  // A function that uses pattern matching to multiply a list of doubles
  def product(ds: List[Double]): Double = ds match {
    case Nil          ⇒ 1.0
    case Cons(0.0, _) ⇒ 0.0
    case Cons(x, xs)  ⇒ x * product(xs)
  }

  // A tail-recursive function that creates a `List` of `n` copies of `a`
  def fill[A](n: Int, a: A): List[A] = {
    @annotation.tailrec
    def loop(n: Int, acc: List[A]): List[A] = {
      n match {
        case 0 ⇒ acc
        case _ ⇒ loop(n - 1, Cons(a, acc))
      }
    }

    loop(n, Nil)
  }

  // A function to append one `List` to another; note that only the first `List` is traversed
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        ⇒ a2
    case Cons(h, t) ⇒ Cons(h, append(t, a2))
  }

  /**
   * Revisiting `sum` and `product`. The operations are identical, save the value returned for an empty `List` (0 vs. 1.0)
   * and the operation to combine results (+ vs. *). Generalize the logic by pulling these two out.
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = as match {
    case Nil        ⇒ z
    case Cons(h, t) ⇒ f(h, foldRight(t, z)(f))
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)((x, y) ⇒ x + y) // initialize with 0 and add to combine
  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _) // initialize with 1.0 and multiply to combine

  // Exercise 3.2 - Implement the `tail` function for removing the first element of a `List`
  def tail[A](l: List[A]): List[A] = l match {
    case Nil        ⇒ sys.error("tail of empty list")
    case Cons(_, t) ⇒ t
  }

  // Exercise 3.3 - Implement the `setHead` function for replacing the first element of a `List` with a different value
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil        ⇒ sys.error("setHead on empty list")
    case Cons(_, t) ⇒ Cons(a, t)
  }

  //  // Exercise 3.4 - Generalize `tail` to the function `drop`, which removes the first _n_ elements from a list.
  //  //
  //  // Not optimal; the tail recursive loop is fine, but this function relies on the `tail` function
  //  // already being implemented, and it throws an error if the size of `n` is greater than the
  //  // size of the `List`. The error is the `tail` error, and so it's a little misleading
  //  def drop[A](l: List[A], n: Int): List[A] = {
  //    @annotation.tailrec
  //    def loop(n: Int, acc: List[A]): List[A] = n match {
  //      case 0 ⇒ acc
  //      case _ ⇒ loop(n - 1, tail(acc))
  //    }
  //
  //    loop(n, l)
  //  }

  // Exercise 3.4 - Generalize `tail` to the function `drop`, which removes the first _n_ elements from a list.
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil        ⇒ Nil
      case Cons(_, t) ⇒ drop(t, n - 1)
    }
  }

  // Exercise 3.5 - Implement `dropWhile`, which removes elements from the `List` prefix as long as they match a predicate.
  //
  // Uses a `pattern guard` to test the match
  def dropWhile[A](l: List[A], f: A ⇒ Boolean): List[A] = l match {
    case Cons(h, t) if f(h) ⇒ dropWhile(t, f)
    case _                  ⇒ l
  }

  // Exercise 3.6 - Implement a function, `init`, that returns a `List` consisting of all but the last element of a `List`.
  //
  // Version 1; this uses an immutable accumulator to copy elements from the `List` over.
  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], acc: List[A]): List[A] = l match {
      case Nil          ⇒ sys.error("init of empty list")
      case Cons(_, Nil) ⇒ acc
      case Cons(h, t)   ⇒ loop(t, Cons(h, acc))
    }

    loop(l, Nil)
  }

  // Exercise 3.6 - Implement a function, `init`, that returns a `List` consisting of all but the last element of a `List`.
  //
  // Version 2; this uses a mutable `ListBuffer` to copy elements from the `List` over.
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buffer = new ListBuffer[A]

    @annotation.tailrec
    def loop(l: List[A]): List[A] = l match {
      case Nil          ⇒ sys.error("init of empty list")
      case Cons(_, Nil) ⇒ List(buffer.toList: _*)
      case Cons(h, t)   ⇒ buffer += h; loop(t)
    }

    loop(l)
  }

  /**
   * Exercise 3.7
   *
   * The `product` function, as implemented by `foldRight` cannot halt the recursion
   * if it encounters a 0.0 because the entire `List` is traversed before any call
   * to the function `f` is computed:
   *
   * foldRight(List(1, 2, 3, 0), 1.0)(_ * _)
   *
   * evaluates as:
   *
   * (1 * foldRight(List(2, 3, 0), 1.0)(_ * _))
   * (1 * (2 * foldRight(List(3, 0), 1.0)(_ * _)))
   * (1 * (2 * (3 * foldRight(List(0), 1.0)(_ * _))))
   * (1 * (2 * (3 * (0 * foldRight(Nil, 1.0)(_ * _)))))
   * (1 * (2 * (3 * (0 * 1.0))))
   *
   * In order to terminate early, this would require non-strict evaluation, so that
   * the function could be applied before evaluating its argument.
   */

  /**
   * Exercise 3.8
   *
   * foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil: List[Int])(Cons(_,_))
   *
   * evaluates as:
   *
   * Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil: List[Int])(Cons(_,_)))
   * Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil: List[Int])(Cons(_,_))))
   * Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil: List[Int])(Cons(_,_)))))
   * Cons(1, Cons(2, Cons(3, Nil)))
   *
   * This is because `foldRight` replaces the `Nil` of `List` with the
   * initialization value, and the `Cons` of `List` with the combining
   * function. In the case of summation, those are `0` and `+`,
   * respectively. In the case of `Nil` and `Cons`, we just get back
   * the input `List`.
   */

  // Exercise 3.9 - Compute the length of a list using `foldRight`.
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, y) ⇒ y + 1)

  //  // Exercise 3.10 - Write another general list-recursion function, `foldLeft`, that is tail-recursive.
  //  def foldLeft[A, B](as: List[A], b: B)(f: (B, A) ⇒ B): B = {
  //    @annotation.tailrec
  //    def loop(as: List[A], acc: B): B = as match {
  //      case Nil         ⇒ acc
  //      case Cons(x, xs) ⇒ loop(xs, f(acc, x))
  //    }
  //
  //    loop(as, b)
  //  }

  // Exercise 3.10 - Write another general list-recursion function, `foldLeft`, that is tail-recursive.
  //
  // Note that defining an inner loop is not needed; we can match the input `List` to return `b` in the
  // empty case, and recursively call `foldLeft` in the nonempty case, passing the tail of our `List`
  // and the result of applying `f` to our `b` and the head of the `List` as our accumulator.
  //
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], b: B)(f: (B, A) ⇒ B): B = as match {
    case Nil         ⇒ b
    case Cons(x, xs) ⇒ foldLeft(xs, f(b, x))(f)
  }

  // Exercise 3.11 - Write `sum`, `product`, and a function to compute the length of a list using `foldLeft`.
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def length3[A](l: List[A]): Int = foldLeft(l, 0)((x, _) ⇒ x + 1)

  // Exercise 3.12 - Write a function that returns the reverse of a `List`.
  def reverse[A](l: List[A]) = foldLeft(l, Nil: List[A])((acc, i) ⇒ Cons(i, acc))

  /* Exercise 3.13 - Implement `foldLeft` in terms of `foldRight` and vice versa. */

  /**
   * This implementation uses `reverse` to mimic the application of `foldRight` in a `foldLeft`. This is a
   * common trick for avoiding stack overflows when implementing a string `foldRight` operation.
   */
  def foldRightViaFoldLeft1[A, B](l: List[A], z: B)(f: (A, B) ⇒ B): B =
    foldLeft(reverse(l), z)((b, a) ⇒ f(a, b))

  /**
   * This implementation uses a delay function to aggregate over successive iterations. See the explanation
   * in the answer section for more detail. Initialize with the identity function and accumulate by applying
   * each derived function to the next element on the `List`.
   */
  def foldRightViaFoldLeft2[A, B](l: List[A], z: B)(f: (A, B) ⇒ B): B =
    foldLeft(l, (b: B) ⇒ b)((g, a) ⇒ b ⇒ g(f(a, b)))(z)

  /**
   * To implement `foldLeft` in terms of `foldRight` we do the same thing as above. See the explanation in
   * the answer section for more detail. Initialize with the identity function and accumulate by applying
   * each derived function to the next element on the `List`.
   */
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) ⇒ B): B =
    foldRight(l, (b: B) ⇒ b)((a, g) ⇒ b ⇒ g(f(b, a)))(z)

  // Exercise 3.14 - Implement `append` in terms of either `foldLeft` or `foldRight`.
  def appendViaFoldLeft[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(reverse(l1), l2)((acc, x) ⇒ Cons(x, acc))

  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((x, acc) ⇒ Cons(x, acc))

  /**
   * Exercise 3.15 - Hard: Write a function that concatenates a list of lists into a single list.
   * Its runtime should be linear in the total length of all lists. Try to use functions we have
   * already defined.
   */
  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(append)

  /**
   * Exercise 3.16 - Write a function that transforms a list of integers by adding 1 to each
   * element. (Reminder: this should be a pure function that returns a new List!)
   */
  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((x, acc) ⇒ Cons(x + 1, acc))

  /**
   * Exercise 3.17 - Write a function that turns each value in a `List[Double]` into a
   * `String`. You can use the expression `d.toString` to convert some `d: Double` to
   * a `String`.
   */
  def doublesToStrings(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((h, t) ⇒ Cons(h.toString, t))

  /**
   * Exercise 3.18 - Write a function `map` that generalizes modifying each element in a
   * list while maintaining the structure of the list.
   */
  //    def map[A, B](as: List[A])(f: A ⇒ B): List[B] =
  //      foldRight(as, Nil: List[B])((h, t) ⇒ Cons(f(h), t))

  // Variant 2; since `foldRight` is not stack-safe, we could use our implementation in terms of `foldLeft`
  def map[A, B](as: List[A])(f: A ⇒ B): List[B] =
    foldRightViaFoldLeft2(as, Nil: List[B])((h, t) ⇒ Cons(f(h), t))

  // Variant 3; uses local mutation to keep a buffer
  //  def map[A, B](as: List[A])(f: A ⇒ B): List[B] = {
  //    import collection.mutable.ListBuffer
  //    val buffer = new ListBuffer[B]
  //
  //    @annotation.tailrec
  //    def loop(l: List[A]): Unit = l match {
  //      case Nil ⇒ ()
  //      case Cons(h, t) ⇒ {
  //        buffer += f(h)
  //        loop(t)
  //      }
  //    }
  //
  //    loop(as);
  //    List(buffer.toList: _*) // convert from standard Scala `List` to our implementation
  //  }

  /**
   * Exercise 3.19 - Write a function `filter` that removes elements from a list unless they
   * satisfy a given predicate.
   */
  def filter[A](l: List[A])(f: A ⇒ Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) ⇒ if (f(h)) Cons(h, t) else t)

  // Variant 2; since `foldRight` is not stack-safe, use the safe version.
  //  def filter[A](l: List[A])(f: A ⇒ Boolean): List[A] =
  //    foldRight(l, Nil: List[A])((h, t) ⇒ if (f(h)) Cons(h, t) else t)

  // Variant 3; uses local mutation to build up a buffer
  //  def filter[A](l: List[A])(f: A ⇒ Boolean): List[A] = {
  //    import collection.mutable.ListBuffer
  //    val buffer = new ListBuffer[A]
  //
  //    @annotation.tailrec
  //    def loop(l: List[A]): Unit = l match {
  //      case Nil        ⇒ ()
  //      case Cons(h, t) ⇒ if (f(h)) buffer += h; loop(t)
  //    }
  //
  //    loop(l)
  //    List(buffer.toList: _*)
  //  }

  /**
   * Exercise 3.20 - Write a function `flatMap` that works like map except that the function
   * given will return a list instead of a single result, and that list should be inserted
   * into the final resulting list.
   */
  def flatMap[A, B](l: List[A])(f: A ⇒ List[B]): List[B] =
    concat(map(l)(f))

  // Variant 2; implemented directly via `foldRight`
  //  def flatMap[A, B](l: List[A])(f: A ⇒ List[B]): List[B] =
  //    foldRightViaFoldLeft2(l, Nil: List[B])((h, t) ⇒ append(f(h), t))

  /**
   * Exercise 3.21 - Use `flatMap` to implement `filter`
   */
  def filterViaFlatMap[A](l: List[A])(f: A ⇒ Boolean): List[A] =
    flatMap(l)(x ⇒ if (f(x)) List(x) else Nil)

  /**
   * Exercise 3.22 - Write a function that accepts two lists and constructs a new list by adding
   * corresponding elements.
   */
  def zipAdd(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _)                     ⇒ Nil
    case (_, Nil)                     ⇒ Nil
    case (Cons(h1, t1), Cons(h2, t2)) ⇒ Cons(h1 + h2, zipAdd(t1, t2))
  }

  /**
   * Exercise 3.23 - Generalize the function you just wrote so that it’s not specific to integers
   * or addition.
   */
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) ⇒ C): List[C] = (a, b) match {
    case (Nil, _)                     ⇒ Nil
    case (_, Nil)                     ⇒ Nil
    case (Cons(h1, t1), Cons(h2, t2)) ⇒ Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  /**
   * Exercise 3.24 - Implement `hasSubsequence` for checking whether a `List` contains another `List` as a subsequence.
   * For instance, `List(1,2,3,4)` would have `List(1,2)`, `List(2,3)`, and `List(4)` as subsequences, among others.
   * You may have some difficulty finding a concise purely functional implementation that is also efficient. That’s
   * okay. Implement the function however comes most naturally. We’ll return to this implementation in chapter 5 and
   * hopefully improve on it. Note: Any two values `x` and `y` can be compared for equality in Scala using the
   * expression `x == y`.
   */

  // First, define a `startsWith` method that tests whether a `List[A]` contains another `List[A]` as a prefix.
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil)                                 ⇒ true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 ⇒ startsWith(t1, t2)
    case _                                        ⇒ false
  }

  // Now, define our subsequence method to recursively process tails of the containing list.
  @annotation.tailrec
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil                       ⇒ sub == Nil
    case _ if startsWith(sup, sub) ⇒ true
    case Cons(_, t)                ⇒ hasSubSequence(t, sub)
  }

  // Variadic function syntax;
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //  val x = List(1, 2, 3, 4, 5) match {
  //    case Cons(x, Cons(2, Cons(4, _)))          ⇒ x
  //    case Nil                                   ⇒ 42
  //    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) ⇒ x + y
  //    case Cons(h, t)                            ⇒ h + sum(t)
  //    case _                                     ⇒ 101
  //  }
  //
  //  def length[A](l: List[A]): Int = sys.error("todo")
  //
  //  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) ⇒ B): B = sys.error("todo")
  //
  //  def map[A, B](l: List[A])(f: A ⇒ B): List[B] = sys.error("todo")
}

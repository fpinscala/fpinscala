package fpinscala.datastructures


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // Variadic function syntax
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  /** Returns the sum of a list of integers. */
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  /** Returns the product of a list of doubles. */
  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  /** Append one list to another. */
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /** Iterate over elements of this list to fill an accumulated value. */
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Cons(h, t) => f(h, foldRight(t, z)(f))
    case Nil        => z
  }

  /**
    * Exercise 3.1
    *
    * What will be the result of the following match expression?
    */
  val x: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // <- this is the first matching
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  /**
    * Exercise 3.2
    *
    * Implement the function tail for removing the first element of a List. Note that the function
    * takes constant time. What are different choices you could make in your implementation if the
    * List is Nil? We’ll return to this question in the next chapter.
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil        => sys.error("tail of empty list")
  }

  /**
    * Exercise 3.3
    *
    * Using the same idea, implement the function setHead for replacing the first element of a List
    * with a different value.
    */
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Cons(_, t) => Cons(a, t)
    case Nil        => sys.error("setHead of empty list")
  }

  /**
    * Exercise 3.4
    *
    * Generalize tail to the function drop, which removes the first n elements from a list. Note
    * that this function takes time proportional only to the number of elements being dropped - we
    * don’t need to make a copy of the entire List.
    */
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, k) if k <= 0 => l
    case (Nil, _)         => Nil
    case (Cons(_, t), _)  => drop(t, n - 1)
  }

  /**
    * Exercise 3.5
    *
    * Implement dropWhile, which removes elements from the List prefix as long as they match a
    * predicate.
    */
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _                  => l
  }

  /**
    * Exercise 3.6
    *
    * Not everything works out so nicely. Implement a function, init, that returns a List consisting
    * of all but the last element of a List. So, given List(1,2,3,4), init will return List(1,2,3).
    * Why can’t this function be implemented in constant time like tail?
    *
    * ==Answer==
    * This function can't be implemented in constant time because our ADT for List is
    * right-associative. This means that the entire list must be traversed in order to return all
    * but the last element.
    */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
    case Nil          => sys.error("init of empty list")
  }

  /** Stack-safe version of init */
  def init2[A](l: List[A]): List[A] = {
    val buffer = new scala.collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def loop(as: List[A]): List[A] = as match {
      case Cons(_, Nil) => List(buffer: _*)
      case Cons(h, t)   => buffer += h; loop(t)
      case Nil          => sys.error("init2 of empty list")
    }

    loop(l)
  }

  /**
    * Exercise 3.7
    *
    * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it
    * encounters a 0.0? Why or why not? Consider how any short-circuiting might work if you call
    * foldRight with a large list. This is a deeper question that we’ll return to in chapter 5
    *
    * ==Answer==
    * No, because foldRight is right-associative. This means that the entire list must be traversed
    * before evaluation of f begins. Hence, no short-circuiting is possible.
    */

  /**
    * Exercise 3.8
    *
    * See what happens when you pass Nil and Cons themselves to foldRight, like this:
    * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)). What do you think this says about the
    * relationship between foldRight and the data constructors of List?
    *
    * ==Answer==
    * We get the original list back, as below:
    *
    * {{{
    *   scala> val xs = List(1, 2, 3)
    *   xs: fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
    *
    *   scala> foldRight(xs, Nil: List[Int])(Cons(_, _))
    *   res0: fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
    * }}}
    *
    * One way of thinking about what foldRight does, is that it replaces the Nil constructor of a
    * list with the z argument, and it replaces the Cons constructor with the function f. If we pass
    * Nil and Cons themselves as z and f (respectively), then we simply get the input list back.
    */

  /**
    * Exercise 3.9
    *
    * Compute the length of a list using foldRight.
    */
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, x) => x + 1)

  /**
    * Exercise 3.10
    *
    * Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError
    * for large lists (we say it’s not stack-safe). Convince yourself that this is the case, and
    * then write another general list-recursion function, foldLeft, that is tail-recursive, using
    * the techniques we discussed in the previous chapter.
    */
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case Nil        => z
  }

  /**
    * Exercise 3.11
    *
    * Write sum, product, and a function to compute the length of a list using foldLeft.
    */
  def sumViaFoldLeft(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def productViaFoldLeft(doubles: List[Double]): Double =
    foldLeft(doubles, 1.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((x, _) => x + 1)

  /**
    * Exercise 3.12
    *
    * Write a function that returns the reverse of a list (given List(1,2,3) it returns
    * List(3,2,1)). See if you can write it using a fold.
    */
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((t, a) => Cons(a, t))

  /**
    * Exercise 3.13
    *
    * Hard: Can you write foldLeft in terms of foldRight? How about the other way around?
    * Implementing foldRight via foldLeft is useful because it lets us implement foldRight
    * tail-recursively, which means it works even for large lists without overflowing the stack.
    *
    * ==Note==
    * To illustrate the differences between foldRight and foldLeft let's follow an example
    * derivation for constructing a list:
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
    * Note that foldLeft processes list elements in the reverse order from foldRight. In order to
    * mimic this, we wrap each operation in a function to delay evaluation until later, and stack
    * the functions so that the order of operation can be reversed.
    *
    * Derivation of the above example using this method proceeds as follows:
    *
    * foldRightViaFoldLeft(List(1, 2, 3), Nil)((a, acc) => Cons(a, acc))
    *
    * becomes:
    * foldLeft(List(1, 2, 3), identity)(combiner)(Nil)
    *
    * becomes:
    * val delay3: BtoB = b => identity(Cons(1, b))
    * val delay2: BtoB = b => delay3(Cons(2, b))
    * val delay1: BtoB = b => delay2(Cons(3, b))
    * delay1(Nil)
    *
    * becomes:
    * val delay3: BtoB = b => identity(Cons(1, b))
    * val delay2: BtoB = b => delay3(Cons(2, b))
    * delay2(Cons(3, Nil))
    *
    * becomes:
    * val delay3: BtoB = b => identity(Cons(1, b))
    * delay3(Cons(2, Cons(3, Nil)))
    *
    * becomes:
    * identity(Cons(1, Cons(2, Cons(3, Nil))))
    *
    * becomes:
    * Cons(1, Cons(2, Cons(3, Nil)))
    */
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    // type alias for convenience
    type BtoB = B => B

    // function to delay evaluation; identity has the right (i.e., no) semantics
    def identity: BtoB = b => b

    // combining function; wraps application of f inside the delaying function
    def combiner: (BtoB, A) => BtoB = (g, a) => b => g(f(a, b))

    // use foldLeft to build the stacked function calls
    val delayed = foldLeft(l, identity)(combiner)

    // trigger evaluation
    delayed(z)
  }

  /** Version of foldLeft implemented in terms of foldRight for illustration */
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  /**
    * Exercise 3.14
    *
    * Implement append in terms of either foldLeft or foldRight.
    */
  def appendViaFoldRight[A](a: List[A], b: List[A]): List[A] =
    foldRight(a, b)(Cons(_, _))

  /** Version in terms of foldLeft for illustration */
  def appendViaFoldLeft[A](a: List[A], b: List[A]): List[A] =
    foldLeft(reverse(a), b)((acc, a) => Cons(a, acc))

  /**
    * Exercise 3.15
    *
    * Hard: Write a function that concatenates a list of lists into a single list. Its runtime
    * should be linear in the total length of all lists. Try to use functions we have already
    * defined.
    */
  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(appendViaFoldRight)

  /**
    * Exercise 3.16
    *
    * Write a function that transforms a list of integers by adding 1 to each element. (Reminder:
    * this should be a pure function that returns a new List!)
    */
  def addOne(is: List[Int]): List[Int] =
    foldRight(is, Nil: List[Int])((i, acc) => Cons(i + 1, acc))

  /**
    * Exercise 3.17
    *
    * Write a function that turns each value in a List[Double] into a String. You can use the
    * expression d.toString to convert some d: Double to a String.
    */
  def doublesToStrings(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((d, acc) => Cons(d.toString, acc))

  /**
    * Exercise 3.18
    *
    * Write a function map that generalizes modifying each element in a list while maintaining the
    * structure of the list.
    */
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, acc) => Cons(f(a), acc))

  /** Version in terms of foldLeft for illustration */
  def mapViaFoldLeft[A, B](l: List[A])(f: A => B): List[B] =
    reverse(foldLeft(l, Nil: List[B])((acc, a) => Cons(f(a), acc)))

  /**
    * Exercise 3.19
    *
    * Write a function filter that removes elements from a list unless they satisfy a given
    * predicate. Use it to remove all odd numbers from a List[Int].
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  /**
    * Exercise 3.20
    *
    * Write a function flatMap that works like map except that the function given will return a list
    * instead of a single result, and that list should be inserted into the final resulting list.
    */
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  /**
    * Exercise 3.21
    *
    * Use flatMap to implement filter.
    */
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  /**
    * Exercise 3.22
    *
    * Write a function that accepts two lists and constructs a new list by adding corresponding
    * elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    */
  def addPairs(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (_, Nil)                     => Nil
    case (Nil, _)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairs(t1, t2))
  }

  /**
    * Exercise 3.23
    *
    * Generalize the function you just wrote so that it’s not specific to integers or addition. Name
    * your generalized function zipWith.
    */
  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
    case (_, Nil)                     => Nil
    case (Nil, _)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  /**
    * Exercise 3.24
    *
    * Hard: As an example, implement hasSubsequence for checking whether a List contains another
    * List as a subsequence. For instance, List(1,2,3,4) would have List(1,2), List(2,3), and
    * List(4) as subsequences, among others. You may have some difficulty finding a concise purely
    * functional implementation that is also efficient. That’s okay. Implement the function however
    * comes most naturally. We’ll return to this implementation in chapter 5 and hopefully improve
    * on it. Note: Any two values x and y can be compared for equality in Scala using the expression
    * x == y.
    */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    // returns true if b is a prefix of a
    @annotation.tailrec
    def startsWith(a: List[A], b: List[A]): Boolean = (a, b) match {
      case (_, Nil)                                 => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _                                        => false
    }

    // recursively test if sub is a prefix of (tails of) sup
    sup match {
      case Nil                       => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t)                => hasSubsequence(t, sub)
    }
  }
}

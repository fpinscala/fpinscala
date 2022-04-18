package fpinscala.answers.datastructures

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
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: MyList[Double]): Double = ds match
    case MyNil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)

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

  def foldRight[A, B](as: MyList[A], z: B, f: (A, B) => B): B = // Utility functions
    as match
      case MyNil => z
      case Cons(x, xs) => f(x, foldRight(xs, z, f))

  def sumViaFoldRight(ns: MyList[Int]) =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: MyList[Double]) =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /*
  3. The third case is the first that matches, with `x` bound to 1 and `y` bound to 2.
  */

  /*
  Although we could return `MyNil` when the input list is empty, we choose to throw an exception instead. This is
  a somewhat subjective choice. In our experience, taking the tail of an empty list is often a bug, and silently
  returning a value just means this bug will be discovered later, further from the place where it was introduced.

  It's generally good practice when pattern matching to use `_` for any variables you don't intend to use on the
  right hand side of a pattern. This makes it clear the value isn't relevant.
  */
  def tail[A](l: MyList[A]): MyList[A] =
    l match
      case MyNil => sys.error("tail of empty list")
      case Cons(_, t) => t

  /*
  If a function body consists solely of a match expression, we'll often put the match on the same line as the
  function signature, rather than introducing another level of nesting.
  */
  def setHead[A](l: MyList[A], h: A): MyList[A] = l match
    case MyNil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)

  /*
  Again, it's somewhat subjective whether to throw an exception when asked to drop more elements than the list
  contains. The usual default for `drop` is not to throw an exception, since it's typically used in cases where this
  is not indicative of a programming error. If you pay attention to how you use `drop`, it's often in cases where the
  length of the input list is unknown, and the number of elements to be dropped is being computed from something else.
  If `drop` threw an exception, we'd have to first compute or check the length and only drop up to that many elements.
  */
  def drop[A](l: MyList[A], n: Int): MyList[A] =
    if n <= 0 then l
    else l match
      case MyNil => MyNil
      case Cons(_,t) => drop(t, n-1)

  /*
  Somewhat overkill, but to illustrate the feature we're using a _pattern guard_, to only match a `Cons` whose head
  satisfies our predicate, `f`. The syntax is to add `if <cond>` after the pattern, before the `=>`, where `<cond>` can
  use any of the variables introduced by the pattern.
  */
  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] =
    l match
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l

  /*
  Note that we're copying the entire list up until the last element. Besides being inefficient, the natural recursive
  solution will use a stack frame for each element of the list, which can lead to stack overflows for
  large lists (can you see why?). With lists, it's common to use a temporary, mutable buffer internal to the
  function (with lazy lists or streams, which we discuss in chapter 5, we don't normally do this). So long as the
  buffer is allocated internal to the function, the mutation is not observable and RT is preserved.

  Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which
  doesn't require even local mutation. We'll write a reverse function later in this chapter.
  */
  def init[A](l: MyList[A]): MyList[A] =
    l match
      case MyNil => sys.error("init of empty list")
      case Cons(_, MyNil) => MyNil
      case Cons(h, t) => Cons(h, init(t))

  def init2[A](l: MyList[A]): MyList[A] =
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: MyList[A]): MyList[A] = cur match
      case MyNil => sys.error("init of empty list")
      case Cons(_, MyNil) => MyList(buf.toList*)
      case Cons(h, t) => buf += h; go(t)
    go(l)

  /*
  No, this is not possible! The reason is because _before_ we ever call our function, `f`, we evaluate its argument,
  which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation
  to support early termination---we discuss this in chapter 5.
  */

  /*
  We get back the original list! Why is that? As we mentioned earlier, one way of thinking about what `foldRight` "does"
  is it replaces the `Nil` constructor of the list with the `z` argument, and it replaces the `Cons` constructor with
  the given function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`, then we get back the input list.

  foldRight(Cons(1, Cons(2, Cons(3, MyNil))), MyNil: MyList[Int])(Cons(_,_))
  Cons(1, foldRight(Cons(2, Cons(3, MyNil)), MyNil: MyList[Int])(Cons(_,_)))
  Cons(1, Cons(2, foldRight(Cons(3, MyNil), MyNil: MyList[Int])(Cons(_,_))))
  Cons(1, Cons(2, Cons(3, foldRight(MyNil, MyNil: MyList[Int])(Cons(_,_)))))
  Cons(1, Cons(2, Cons(3, MyNil)))
  */

  def length[A](l: MyList[A]): Int =
    foldRight(l, 0, (_,acc) => acc + 1)

  /*
  It's common practice to annotate functions you expect to be tail-recursive with the `tailrec` annotation. If the
  function is not tail-recursive, it will yield a compile error, rather than silently compiling the code and resulting
  in greater stack space usage at runtime.
  */
  @annotation.tailrec
  def foldLeft[A, B](l: MyList[A], acc: B, f: (B, A) => B): B = l match
    case MyNil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(l: MyList[Int]) = foldLeft(l, 0, _ + _)
  def productViaFoldLeft(l: MyList[Double]) = foldLeft(l, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: MyList[A]): Int = foldLeft(l, 0, (acc, h) => acc + 1)

  def reverse[A](l: MyList[A]): MyList[A] = foldLeft(l, MyList[A](), (acc, h) => Cons(h, acc))

  /*
  The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack overflows
  when implementing a strict `foldRight` function as we've done in this chapter. (We'll revisit this in a later chapter,
  when we discuss laziness).

  The other implementations build up a chain of functions which, when called, results in the operations being performed
  with the correct associativity. We are calling `foldRight` with the `B` type being instantiated to `B => B`, then
  calling the built up function with the `z` argument. Try expanding the definitions by substituting equals for equals
  using a simple example, like `foldLeft(MyList(1,2,3), 0)(_ + _)` if this isn't clear. Note these implementations are
  more of theoretical interest - they aren't stack-safe and won't work for large lists.
  */
  def foldRightViaFoldLeft[A, B](l: MyList[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(l), acc, (b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: MyList[A], acc: B, f: (A, B) => B): B =
    foldLeft(l, (b: B) => b, (g, a) => b => g(f(a, b)))(acc)

  def foldLeftViaFoldRight[A, B](l: MyList[A], acc: B, f: (B,A) => B): B =
    foldRight(l, (b: B) => b, (a, g) => b => g(f(b, a)))(acc)

  /*
  `append` simply replaces the `Nil` constructor of the first list with the second list, which is exactly the operation
  performed by `foldRight`.
  */
  def appendViaFoldRight[A](l: MyList[A], r: MyList[A]): MyList[A] =
    foldRight(l, r, Cons(_,_))

  /*
  Since `append` takes time proportional to its first argument, and this first argument never grows because of the
  right-associativity of `foldRight`, this function is linear in the total length of all lists. You may want to try
  tracing the execution of the implementation on paper to convince yourself that this works.

  Note that we're simply referencing the `append` function, without writing something like `(x,y) => append(x,y)`
  or `append(_,_)`. In Scala there is a rather arbitrary distinction between functions defined as _methods_, which are
  introduced with the `def` keyword, and function values, which are the first-class objects we can pass to other
  functions, put in collections, and so on. This is a case where Scala lets us pretend the distinction doesn't exist.
  In other cases, you'll be forced to write `append _` (to convert a `def` to a function value)
  or even `(x: MyList[A], y: MyList[A]) => append(x,y)` if the function is polymorphic and the type arguments aren't known.
  */
  def concat[A](l: MyList[MyList[A]]): MyList[A] =
    foldRight(l, MyNil: MyList[A], append)

  def incrementEach(l: MyList[Int]): MyList[Int] =
    foldRight(l, MyNil: MyList[Int], (i, acc) => Cons(i + 1, acc))

  def doubleToString(l: MyList[Double]): MyList[String] =
    foldRight(l, MyNil: MyList[String], (d, acc) => Cons(d.toString, acc))

  /*
  A natural solution is using `foldRight`, but our implementation of `foldRight` is not stack-safe. We can
  use `foldRightViaFoldLeft` to avoid the stack overflow (variation 1), but more commonly, with our current
  implementation of `MyList`, `map` will just be implemented using local mutation (variation 2). Again, note that the
  mutation isn't observable outside the function, since we're only mutating a buffer that we've allocated.
  */
  def map[A, B](l: MyList[A])(f: A => B): MyList[B] =
    foldRight(l, MyNil: MyList[B], (h,t) => Cons(f(h), t))

  def map_1[A, B](l: MyList[A])(f: A => B): MyList[B] =
    foldRightViaFoldLeft(l, MyNil: MyList[B], (h,t) => Cons(f(h), t))

  def map_2[A, B](l: MyList[A])(f: A => B): MyList[B] =
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: MyList[A]): Unit = l match
      case MyNil => ()
      case Cons(h, t) => buf += f(h); go(t)
    go(l)
    MyList(buf.toList*) // converting from the standard Scala list to the list we've defined here

  /*
  The discussion about `map` also applies here.
  */
  def filter[A](l: MyList[A])(f: A => Boolean): MyList[A] =
    foldRight(l, MyNil: MyList[A], (h,t) => if f(h) then Cons(h, t) else t)

  def filter_1[A](l: MyList[A])(f: A => Boolean): MyList[A] =
    foldRightViaFoldLeft(l, MyNil: MyList[A], (h,t) => if f(h) then Cons(h, t) else t)

  def filter_2[A](l: MyList[A])(f: A => Boolean): MyList[A] =
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: MyList[A]): Unit = l match
      case MyNil => ()
      case Cons(h, t) => if f(h) then buf += h; go(t)
    go(l)
    MyList(buf.toList*) // converting from the standard Scala list to the list we've defined here

  /*
  This could also be implemented directly using `foldRight`.
  */
  def flatMap[A, B](l: MyList[A])(f: A => MyList[B]): MyList[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(l)(a => if f(a) then MyList(a) else MyNil)

  /*
  To match on multiple values, we can put the values into a pair and match on the pair, as shown next, and the same
  syntax extends to matching on N values (see sidebar "Pairs and tuples in Scala" for more about pair and tuple
  objects). You can also (somewhat less conveniently, but a bit more efficiently) nest pattern matches: on the
  right hand side of the `=>`, simply begin another `match` expression. The inner `match` will have access to all the
  variables introduced in the outer `match`.

  The discussion about stack usage from the explanation of `map` also applies here.
  */
  def addPairwise(a: MyList[Int], b: MyList[Int]): MyList[Int] = (a,b) match
    case (MyNil, _) => MyNil
    case (_, MyNil) => MyNil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))

  /*
  This function is usually called `zipWith`. The discussion about stack usage from the explanation of `map` also
  applies here. By putting the `f` in the second argument list, Scala can infer its type from the previous argument list.
  */
  def zipWith[A, B,C](a: MyList[A], b: MyList[B], f: (A, B) => C): MyList[C] = (a,b) match
    case (MyNil, _) => MyNil
    case (_, MyNil) => MyNil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))

  /*
  Tail recursive implementation of `zipWith`. 
  */
  def zipWith_1[A, B,C](a: MyList[A], b: MyList[B], f: (A, B) => C): MyList[C] =
    @annotation.tailrec
    def loop(a: MyList[A], b: MyList[B], acc: MyList[C]): MyList[C] =
      (a, b) match
        case (MyNil, _) => acc
        case (_, MyNil) => acc
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), acc))
    reverse(loop(a, b, MyNil))

  /*
  There's nothing particularly bad about this implementation,
  except that it's somewhat monolithic and easy to get wrong.
  Where possible, we prefer to assemble functions like this using
  combinations of other functions. It makes the code more obviously
  correct and easier to read and understand. Notice that in this
  implementation we need special purpose logic to break out of our
  loops early. In Chapter 5 we'll discuss ways of composing functions
  like this from simpler components, without giving up the efficiency
  of having the resulting functions work in one pass over the data.
  
  It's good to specify some properties about these functions.
  For example, do you expect these expressions to be true?
  
  (xs append ys) startsWith xs
  xs startsWith MyNil
  (xs append ys append zs) hasSubsequence ys
  xs hasSubsequence MyNil

  */
  @annotation.tailrec
  def startsWith[A](l: MyList[A], prefix: MyList[A]): Boolean = (l,prefix) match
    case (_,MyNil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false

  @annotation.tailrec
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = sup match
    case MyNil => sub == MyNil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)

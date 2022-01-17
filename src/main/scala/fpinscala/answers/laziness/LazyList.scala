package fpinscala.answers.laziness

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  // The natural recursive solution
  def toListRecursive: List[A] = this match
    case Cons(h,t) => h() :: t().toListRecursive
    case Empty => Nil

  /*
  The above solution will stack overflow for large lazy lists, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the lazy list. Then at the end we reverse the result to get the
  correct order again.
  */
  def toList: List[A] =
    @annotation.tailrec
    def go(ll: LazyList[A], acc: List[A]): List[A] = ll match
      case Cons(h, t) => go(t(), h() :: acc)
      case Empty => acc.reverse
    go(this, Nil)

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */
  def toListFast: List[A] =
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(ll: LazyList[A]): List[A] = ll match
      case Cons(h, t) =>
        buf += h()
        go(t())
      case Empty => buf.toList
    go(this)

  /*
    Create a new LazyList[A] from taking the n first elements from this. We can achieve that by recursively
    calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
    we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
    at the lazy list at all.
  */
  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty

  /*
    Create a new LazyList[A] from this, but ignore the n first elements. This can be achieved by recursively calling
    drop on the invoked tail of a cons cell. Note that the implementation is also tail recursive.
  */
  @annotation.tailrec
  final def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this

  def takeWhile(f: A => Boolean): LazyList[A] = this match
    case Cons(h,t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => empty

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  /*
  Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element is found.
  */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def takeWhile_1(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty[A])((a, acc) => if f(a) then cons(a, acc) else acc)

  def append[A2>:A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, acc) => f(a).append(acc))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some((Some(h1()) -> None) -> (t1() -> Empty))
      case (Empty, Cons(h2, t2)) => Some((None -> Some(h2())) -> (Empty -> t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) -> Some(h2())) -> (t1() -> t2()))
    }

  def zipWith[B,C](that: LazyList[B])(f: (A,B) => C): LazyList[C] =
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[B](that: LazyList[B]): LazyList[(A,B)] =
    zipWith(that)((_,_))

  def zipWithAll[B, C](that: LazyList[B])(f: (Option[A], Option[B]) => C): LazyList[C] =
    LazyList.unfold((this, that)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def zipAllViaZipWithAll[B](s2: LazyList[B]): LazyList[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  /*
  `s.startsWith(s2)` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness, we can compose these three separate logical steps--the zipping, the termination when the second lazy list is exhausted, and the termination if a nonmatching element is found or the first lazy list is exhausted.
  */
  def startsWith[A](prefix: LazyList[A]): Boolean =
    zipAll(prefix).takeWhile(_(1).isDefined).forAll { case (a1, a2) => a1 == a2 }

  /*
  The last element of `tails` is always the empty `LazyList`, so we handle this as a special case, by appending it to the output.
  */
  def tails: LazyList[LazyList[A]] =
    unfold(this) {
      case Empty => None
      case l @ Cons(_, t) => Some((l, t()))
    }.append(LazyList(empty))

  def hasSubsequence[A](s: LazyList[A]): Boolean =
    tails.exists(_.startsWith(s))

  /*
  The function can't be implemented using `unfold`, since `unfold` generates elements of the `LazyList` from left to right. It can be implemented using `foldRight` though.

  The implementation is just a `foldRight` that keeps the accumulated value and the lazy list of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
  */
  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    foldRight(init -> LazyList(init)) { (a, b0) =>
      // b0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val b1 = b0
      val b2 = f(a, b1._1)
      (b2, cons(b2, b1._2))
    }._2

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  // This is more efficient than `cons(a, continually(a))` since it's just
  // one object referencing itself.
  def continually[A](a: A): LazyList[A] =
    lazy val single: LazyList[A] = cons(a, single)
    single

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  val fibs =
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(next, current + next))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty

  /*
  The below two implementations use `fold` and `map` functions in the Option class to implement unfold, thereby doing away with the need to manually pattern match as in the above solution.
   */
  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(z).fold(empty[A])((p: (A,S)) => cons(p._1,unfold(p._2)(f)))

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(z).map((p: (A,S)) => cons(p._1,unfold(p._2)(f))).getOrElse(empty[A])

  /*
  Scala provides shorter syntax when the first action of a function literal is to match on an expression.  The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p => p match { case (f0,f1) => ... }`, but we avoid having to choose a name for `p`, only to pattern match on it.
  */
  val fibsViaUnfold: LazyList[Int] =
    unfold((0,1)) { case (current, next) =>
      Some((current, (next, current + next)))
    }

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some((a, ())))

  // could also of course be implemented as constant(1)
  val onesViaUnfold: LazyList[Int] =
    unfold(())(_ => Some((1, ())))

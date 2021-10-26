package fpinscala.monoids

import fpinscala.parallelism.Nonblocking.*
import language.higherKinds

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid = new Monoid[String]:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A] = new Monoid[List[A]]:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  val intAddition: Monoid[Int] = new Monoid[Int]:
    def combine(x: Int, y: Int) = x + y
    val empty = 0

  val intMultiplication: Monoid[Int] = new Monoid[Int]:
    def combine(x: Int, y: Int) = x * y
    val empty = 1

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean]:
    def combine(x: Boolean, y: Boolean) = x || y
    val empty = false

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]:
    def combine(x: Boolean, y: Boolean) = x && y
    val empty = true

  // Notice that we have a choice in how we implement `combine`.
  // We can compose the options in either order. Both of those implementations
  // satisfy the monoid laws, but they are not equivalent.
  // This is true in general--that is, every monoid has a _dual_ where the
  // `combine` combines things in the opposite order. Monoids like `booleanOr` and
  // `intAddition` are equivalent to their duals because their `combine` is commutative
  // as well as associative.
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]:
    def combine(x: Option[A], y: Option[A]) = x orElse y
    val empty = None

  // We can get the dual of any monoid just by flipping the `combine`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A]:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  // Now we can have both monoids on hand
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A]:
    def combine(f: A => A, g: A => A) = f compose g
    val empty = (a: A) => a

  import fpinscala.testing.*
  import Prop.*
  import Gen.*

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    // Associativity
    forAll(gen ** gen ** gen) { case x ** y ** z =>
      m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), y)
     } &&
    // Identity
    forAll(gen)((a: A) =>
      m.combine(a, m.empty) == a && m.combine(m.empty, a) == a)

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  // Notice that this function does not require the use of `map` at all.
  // All we need is `foldLeft`.
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.length == 0 then
      m.empty
    else if as.length == 1 then
      f(as(0))
    else
      val (l, r) = as.splitAt(as.length / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

  // This implementation detects only ascending order,
  // but you can write a monoid that detects both ascending and descending
  // order if you like.
  def ordered(ints: IndexedSeq[Int]): Boolean =
    // Our monoid tracks the minimum and maximum element seen so far
    // as well as whether the elements are so far ordered.
    val mon = new Monoid[Option[(Int, Int, Boolean)]]:
      def combine(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match
          // The ranges should not overlap if the sequence is ordered.
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
      val empty = None
    // The empty sequence is ordered, and each element by itself is ordered.
    foldMapV(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)

  // This ability to 'lift' a monoid any monoid to operate within
  // some context (here `Par`) is something we'll discuss more in
  // chapters 11 & 12
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]:
    def empty = Par.unit(m.empty)
    def combine(a: Par[A], b: Par[A]) = a.map2(b)(m.combine)

  // we perform the mapping and the reducing both in parallel
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  val wcMonoid: Monoid[WC] = new Monoid[WC]:
    import WC.{Stub, Part}
    // The empty result, where we haven't seen any characters yet.
    val empty = Stub("")

    def combine(a: WC, b: WC) = (a, b) match
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if (r1 + l2).isEmpty then 0 else 1) + w2, r2)

  def count(s: String): Int =
    import WC.{Stub, Part}
    // A single character's count. Whitespace does not count,
    // and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if c.isWhitespace then
        Part("", 0, "")
      else
        Stub(c.toString)
    // `unstub(s)` is 0 if `s` is empty, otherwise 1.
    def unstub(s: String) = s.length min 1
    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)]:
      def combine(x: (A, B), y: (A, B)) =
        (A.combine(x._1, y._1), B.combine(x._2, y._2))
      val empty = (A.empty, B.empty)

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B]:
      def combine(f: A => B, g: A => B) = a => B.combine(f(a), g(a))
      val empty: A => B = a => B.empty

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]]:
      def empty = Map[K,V]()
      def combine(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(empty) { (acc,k) =>
          acc.updated(k, V.combine(a.getOrElse(k, V.empty),
                              b.getOrElse(k, V.empty)))
        }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))

end Monoid

trait Foldable[F[_]]:
  import Monoid.*

  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.empty)((a, b) => mb.combine(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.empty)(m.combine)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)

object ListFoldable extends Foldable[List]:
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.empty)((b, a) => mb.combine(b, f(a)))
  override def toList[A](as: List[A]): List[A] = as

object IndexedSeqFoldable extends Foldable[IndexedSeq]:
  import Monoid.*
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)

object LazyListFoldable extends Foldable[LazyList]:
  override def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: LazyList[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object TreeFoldable extends Foldable[Tree]:
  import Tree.{Leaf, Branch}

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.combine(foldMap(l)(f)(mb), foldMap(r)(f)(mb))

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)

// Notice that in `TreeFoldable.foldMap`, we don't actually use the `empty`
// from the `Monoid`. This is because there is no empty tree.
// This suggests that there might be a class of types that are foldable
// with something "smaller" than a monoid, consisting only of an
// associative `combine`. That kind of object (a monoid without a `empty`) is
// called a semigroup. `Tree` itself is not a monoid, but it is a semigroup.

object OptionFoldable extends Foldable[Option]:
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match
      case None => mb.empty
      case Some(a) => f(a)

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match
    case None => z
    case Some(a) => f(z, a)

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match
    case None => z
    case Some(a) => f(a, z)

package fpinscala.monoids

import fpinscala.parallelism.Nonblocking.*
import language.higherKinds

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  val intAddition: Monoid[Int] = new:
    def combine(x: Int, y: Int) = x + y
    val empty = 0

  val intMultiplication: Monoid[Int] = new:
    def combine(x: Int, y: Int) = x * y
    val empty = 1

  val booleanOr: Monoid[Boolean] = new:
    def combine(x: Boolean, y: Boolean) = x || y
    val empty = false

  val booleanAnd: Monoid[Boolean] = new:
    def combine(x: Boolean, y: Boolean) = x && y
    val empty = true

  // Notice that we have a choice in how we implement `combine`.
  // We can compose the options in either order. Both of those implementations
  // satisfy the monoid laws, but they are not equivalent.
  // This is true in general--that is, every monoid has a _dual_ where the
  // `combine` combines things in the opposite order. Monoids like `booleanOr` and
  // `intAddition` are equivalent to their duals because their `combine` is commutative
  // as well as associative.
  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(x: Option[A], y: Option[A]) = x orElse y
    val empty = None

  // We can get the dual of any monoid just by flipping the `combine`.
  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  // Now we can have both monoids on hand
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def combineOptionMonoid[A](f: (A, A) => A): Monoid[Option[A]] = new:
    def combine(x: Option[A], y: Option[A]) = x.map2(y)(f)
    val empty = None

  extension [A](optA: Option[A]) def map2[B, C](optB: Option[B])(f: (A, B) => C): Option[C] =
    for
      a <- optA
      b <- optB
    yield f(a, b)

  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(f: A => A, g: A => A) = f andThen g
    val empty = identity

  import fpinscala.testing.{Prop, Gen}
  import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val associativity = Prop.forAll(gen ** gen ** gen) { case a ** b ** c =>
      m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
    }.tag("associativity")
    val identity = Prop.forAll(gen) { a =>
      m.combine(a, m.empty) == a && m.combine(m.empty, a) == a
    }.tag("identity")
    associativity && identity

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  // Notice that this function does not require the use of `map` at all.
  // All we need is `foldLeft`.
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid)(f.curried)(acc)

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid))(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.length == 0 then
      m.empty
    else if as.length == 1 then
      f(as(0))
    else
      val (l, r) = as.splitAt(as.length / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

  // This ability to 'lift' a monoid any monoid to operate within
  // some context (here `Par`) is something we'll discuss more in
  // chapters 11 & 12
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]:
    def empty = Par.unit(m.empty)
    def combine(a: Par[A], b: Par[A]) = a.map2(b)(m.combine)

  // we perform the mapping and the reducing both in parallel
  def parFoldMap[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(as)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  val orderedMonoid: Monoid[(Boolean, Int)] = new:
    def combine(a1: (Boolean, Int), a2: (Boolean, Int)) =
      (a1(0) && a2(0) && a1(1) <= a2(1), a1(1) max a2(1))
    val empty = (true, Int.MinValue)

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, orderedMonoid)(i => (true, i))(0)

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  val wcMonoid: Monoid[WC] = new Monoid[WC]:
    import WC.{Stub, Part}
    // The empty result, where we haven't seen any characters yet.
    val empty = Stub("")

    def combine(wc1: WC, wc2: WC) = (wc1, wc2) match
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(l, w, r)) => Part(a + l, w, r)
      case (Part(l, w, r), Stub(a)) => Part(l, w, r + a)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if (r1 + l2).isEmpty then 0 else 1) + w2, r2)

  def wcGen: Gen[WC] = 
    val smallString = Gen.choose(0, 10).flatMap(Gen.stringN)
    val genStub = smallString.map(s => WC.Stub(s))
    val genPart = for
      lStub <- smallString 
      words <- Gen.choose(0, 10)
      rStub <- smallString
    yield WC.Part(lStub, words, rStub)
    Gen.union(genStub, genPart)

  val wcMonoidTest = monoidLaws(wcMonoid, wcGen)

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

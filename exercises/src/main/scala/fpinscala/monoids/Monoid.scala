package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.testing.Gen
import fpinscala.testing.Prop.run

import scala.language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override def zero: A = m.zero
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    override def zero: A => A = identity
  }

  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = forAll(gen ** gen ** gen) { case a1 ** a2 ** a3 =>
    m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3))
    m.op(m.zero, a1) == a1
    m.op(a1, m.zero) == a1
  }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap[A, B => B](as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap[A, B => B](as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as.head)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMap(ints.toList, new Monoid[Option[Int]] {
      override def op(a1: Option[Int], a2: Option[Int]): Option[Int] = (a1, a2) match {
        case (Some(n1), Some(n2)) => if (n2 > n1) Some(n2) else None
        case _ => None
      }
      override def zero: Option[Int] = Some(Int.MinValue)
    })(Some(_)).isDefined

  def orderedV(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = (a1, a2) match {
        case (Some((min1, max1, o1)), Some((min2, max2, o2))) => Some(min1, max2, o1 && o2 && min2 > max1)
        case (t, None) => t
        case (None, t) => t
      }
      override def zero: Option[(Int, Int, Boolean)] = None
    })(x => Some(x, x, true)).exists { case (_, _, ordered) => ordered }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val pm = par(m)

    if (v.isEmpty) pm.zero
    else if (v.length == 1) Par.lazyUnit(f(v.head))
    else {
      val (l, r) = v.splitAt(v.length / 2)
      pm.op(parFoldMap(l, m)(f), parFoldMap(r, m)(f))
    }
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(l), Stub(r)) => Stub(l + r)
      case (Stub(l1), Part(l2, w, r)) => Part(l1 + l2, w, r)
      case (Part(l, w, r1), Stub(r2)) => Part(l, w, r1 + r2)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((r1 + l2).nonEmpty) 1 else 0), r2)
    }
    override def zero: WC = Stub("")
  }

  def count(s: String): Int =
    foldMapV(s.toIndexedSeq, wcMonoid)(ch => if (ch == ' ') Part("", 0, "") else Stub(ch.toString)) match {
      case Stub(c) => if (c == "") 0 else 1
      case Part(l, w, r) => w + (if (l == "") 0 else 1) + (if (r == "") 0 else 1)
    }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    override def zero: (A, B) = (A.zero, B.zero)
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))
    override def zero: A => B = _ => B.zero
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
      (a1.keySet ++ a2.keySet).map { k =>
        k -> V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero))
      }.toMap
    override def zero: Map[K, V] = Map.empty
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = foldMapV(as, mapMergeMonoid[A, Int](intAddition))(i => Map(i -> 1))
}

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] = foldMap(as)(_ :: Nil)(listMonoid)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMap(as, mb)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(value) => f(value)
    case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    foldMap[A, B => B](as)(a => b => f(b, a))(Monoid.dual(Monoid.endoMonoid))(z)

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    foldMap[A, B => B](as)(f.curried)(Monoid.endoMonoid)(z)
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(value) => f(value)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(value) => f(z, value)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(value) => f(value, z)
  }
}

object app extends App {

  import fpinscala.monoids.Monoid._

  run(monoidLaws(intAddition, Gen.int))
  run(monoidLaws(intMultiplication, Gen.int))
  run(monoidLaws(booleanAnd, Gen.boolean))
  run(monoidLaws(booleanOr, Gen.boolean))
  run(monoidLaws[Option[Int]](optionMonoid, Gen.trueOption(Gen.int)))
  run(monoidLaws(wcMonoid, Gen.wc(
    Gen.stringN(Gen.choose(0, 100)),
    Gen.choose(0, 123),
    Gen.stringN(Gen.choose(0, 100)))
  ))
  run(monoidLaws[(Int, Boolean)](productMonoid(intAddition, booleanAnd), Gen.int ** Gen.boolean))

  //  run(monoidLaws[Int => Int](endoMonoid, Gen.unit(identity[Int]))) // Gen.endoIntFunction(Gen.int)


  println(ordered((1 :: 3 :: 10 :: 11 :: Nil).toIndexedSeq))
  println(orderedV((1 :: 3 :: 10 :: 11 :: Nil).toIndexedSeq))

  println(ordered((1 :: 10 :: 3 :: 11 :: Nil).toIndexedSeq))
  println(orderedV((1 :: 10 :: 3 :: 11 :: Nil).toIndexedSeq))
  println(ordered((1 :: 3 :: 2 :: Nil).toIndexedSeq))
  println(orderedV((1 :: 3 :: 2 :: Nil).toIndexedSeq))

  println(count("sdlfjskldfj sdfl slkfjskldf s dflsjdflkjd weoi"))
  println(count(""))
  println(count("sdlfjskldfj"))
  println(count("sdlfjskldfj "))

  val t: Branch[String] = Branch(Branch(Leaf("1"), Leaf("2")), Branch(Leaf("3"), Leaf("4")))
  val l = TreeFoldable.foldLeft(t)("")((b, a) => b + a)
  val r = TreeFoldable.foldRight(t)("")((a, b) => a + b)
  println(l)
  println(r)
  println(TreeFoldable.toList(t))

  println(bag((1 :: 2 :: 1 :: 3 :: 2 :: Nil).toIndexedSeq))
  println(bag(("yo" :: "is" :: "a" :: "yo" :: Nil).toIndexedSeq))
}

package fpinscala
package applicative

import monads.Functor
import state._
import State._
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {
  self =>
  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_ (_))

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried))(fa))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(Nil: List[B]))((a, z) => map2(f(a), z)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(Seq.fill(n)(fa).toList)

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???

  def product[G[_]](G: Applicative[G]) = new Applicative[({type f[x] = (F[x], G[x])})#f] {
    override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
    override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
      (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))

  }

  def compose[G[_]](G: Applicative[G]) = new Applicative[({type f[x] = F[G[x]]})#f] {
    override def unit[A](a: => A): F[G[A]] =
      self.unit(G.unit(a))

    override def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] =
      self.map2(fab, fa)(G.apply(_)(_))

    override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
      self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight[F[Map[K, V]]](unit(Map.empty))((kv, z) => map2(kv._2, z)((v, acc) => acc + (kv._1 -> v)))
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  self =>
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] = flatMap(mf)(f => map(ma)(a => f(a)))

  override def map[A, B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))

  def compose[G[_]](G: Monad[G]) = new Monad[({type f[x] = F[G[x]]})#f] {
    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
    override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
      self.flatMap(ma)(mg => ???)
  }
}

object Monad {
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
  Monad[({type f[x] = F[N[x]]})#f] = new Monad[({type f[x] = F[N[x]]})#f] {
    override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))
    override def flatMap[A, B](ma: F[N[A]])(f: A => F[N[B]]): F[N[B]] =
      F.flatMap(ma)(na => F.map(T.traverse(na)(f))(N.join))
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] = Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] = // Combine elements pointwise
      a zip b map f.tupled
  }

  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++: h2 +: t2)
      case (_, e@Failure(_, _)) => e
      case (e@Failure(_, _), _) => e
    }
  }

  val optionApplicative = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = (fa, fb) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Option, A, B](fa)(a => Some(f(a)))(Applicative.optionApplicative).get

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)(
      (a, s) => (
        (a, s), s + 1
      )
    )._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, s) => ((), f(s, a)))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {

    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)
  }

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = new Traverse[({type f[x] = F[G[x]]})#f] {
    override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => M[B]): M[F[G[B]]] =
      self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
  }
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(Nil: List[B]))((a, z) => G.map2(f(a), z)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(value) => G.map(f(value))(Some(_))
        case None => G.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(fa.head), listTraverse.sequence(fa.tail.map(traverse(_)(f))))(Tree(_, _))
  }
}

object app extends App {
  println("yo")

  //  val r = Applicative.streamApplicative.sequence(Stream(1, 2, 3, 4, 5) :: Stream(2, 2, 32, 32, 3, 12, 31, 3, 12, 312, 3) :: Stream(3223, 234, 234, 23, 42, 34) :: Nil)
  //  println(r)

  //  println(Traverse.listTraverse.traverse[Option, Int, Int](1 :: 2 ::3 :: Nil)(n => Some(n))(Applicative.optionApplicative))
  //  println(Traverse.listTraverse.traverse[Option, Int, Int](1 :: 2 ::3 :: Nil)(n => Some(n))(Applicative.optionApplicative))
  //
  //
  //  type IntState[A] = State[Int, A]
  //
  //  val r = Traverse.listTraverse.traverse[IntState, String, Int]("1" :: "22" :: "333" :: Nil)(str => State(st => (str.length, st + 1)) : IntState[Int])(Monad.stateMonad)
  //  println(r.run(0))

  val t = Tree(1,
    Tree(2, Nil) :: Tree(3,
      Tree(4, Nil) :: Tree(5, Nil) :: Tree(6, Nil) :: Nil) :: Nil)
  println(Traverse.treeTraverse.reverse[Int](t))

  println(Traverse.treeTraverse.foldLeft(t)(Nil: List[Int])((z, a) => a :: z))
}
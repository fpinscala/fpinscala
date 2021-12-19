package fpinscala
package applicative

import monads.Functor
import state.*
import State.*
import StateUtil.* // defined at bottom of this file
import monoids.*
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F]:

  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def map2[B,C](fb: F[B])(f: (A, B) => C): F[C] =
      ???

    def map[B](f: A => B): F[B] =
      apply(unit(f))(fa)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    ???

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    ???

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    ???

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    ???

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    ???

  def product[G[_]](G: Applicative[G]): Applicative[[X] =>> (F[X], G[X])] =
    ???

  def compose[G[_]](G: Applicative[G]): Applicative[[X] =>> F[G[X]]] =
    ???

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ???

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F]:
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join
    override def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))
    override def map2[B,C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(f))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

  extension [A](ffa: F[F[A]])
    def join: F[A] = ffa.flatMap(identity)

object Monad {
  def eitherMonad[E]: Monad[Either[E, _]] = ???

  def stateMonad[S] = new Monad[State[S, _]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    extension [A](st: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[[X] =>> F[N[X]]] = ???
}

enum Validation[+E, +A]:
  case Success(a: A)
  case Failure(head: E, tail: Vector[E]) extends Validation[E, Nothing]

object Applicative:
  val lazyListApplicative: Applicative[LazyList] = new:

    def unit[A](a: => A): LazyList[A] =
      LazyList.continually(a) // The infinite, constant stream

    extension [A](a: LazyList[A])
      override def map2[B, C](b: LazyList[B])( // Combine elements pointwise
                              f: (A, B) => C): LazyList[C] =
        a.zip(b).map(f.tupled)


  def validationApplicative[E]: Applicative[Validation[E, _]] = ???

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[Const[M, _]] =
    new Applicative[Const[M, _]] {
      def unit[A](a: => A): M = M.empty
      override def apply[A,B](m1: M)(m2: M): M = M.combine(m1, m2)
    }

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  extension [A](fa: F[A])
    def map[B](f: A => B): F[B] =
      ???

  import Applicative.*

  extension [A](fa: F[A])
    override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      traverse[Const[B, _], A, Nothing](fa)(f)(using monoidApplicative(mb))

    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      ???

    override def toList: List[A] =
      ???

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[State[S, _], A, B](fa)(f)(using Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[[X] =>> F[G[X]]] = ???
}

object Traverse {
  val listTraverse = ???

  val optionTraverse = ???

  val treeTraverse = ???
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}

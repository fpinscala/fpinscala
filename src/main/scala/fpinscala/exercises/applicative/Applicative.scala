package fpinscala.exercises.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.monoids.Monoid
import fpinscala.answers.state.State

trait Applicative[F[_]] extends Functor[F]:
  self =>

  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    ???

  extension [A](fa: F[A])
    def map2[B,C](fb: F[B])(f: (A, B) => C): F[C] =
      ???

    def map[B](f: A => B): F[B] =
      apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    ???

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    ???

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    ???

  extension [A](fa: F[A])
    def product[B](fb: F[B]): F[(A, B)] =
      ???

    def map3[B, C, D](
      fb: F[B],
      fc: F[C]
    )(f: (A, B, C) => D): F[D] =
      ???

    def map4[B, C, D, E](
      fb: F[B],
      fc: F[C],
      fd: F[D]
    )(f: (A, B, C, D) => E): F[E] =
      ???

  def product[G[_]](G: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] =
    ???

  def compose[G[_]](G: Applicative[G]): Applicative[[x] =>> F[G[x]]] =
    ???

  def sequenceMap[K,V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ???

object Applicative:
  opaque type ZipList[+A] = LazyList[A]

  object ZipList:
    def fromLazyList[A](la: LazyList[A]): ZipList[A] = la
    extension [A](za: ZipList[A]) def toLazyList: LazyList[A] = za

    given zipListApplicative: Applicative[ZipList] with
      def unit[A](a: => A): ZipList[A] =
        LazyList.continually(a)
      extension [A](fa: ZipList[A])
        override def map2[B, C](fb: ZipList[B])(f: (A, B) => C) =
          fa.zip(fb).map(f.tupled)

  enum Validated[+E, +A]:
    case Valid(get: A) extends Validated[Nothing, A]
    case Invalid(error: E) extends Validated[E, Nothing]
  
  object Validated:
    given validatedApplicative[E: Monoid]: Applicative[Validated[E, _]] with
      def unit[A](a: => A) = ???
      extension [A](fa: Validated[E, A])
        override def map2[B, C](fb: Validated[E, B])(f: (A, B) => C) =
          ???

  type Const[A, B] = A

  given monoidApplicative[M](using m: Monoid[M]): Applicative[Const[M, _]] with
    def unit[A](a: => A): M = m.empty
    override def apply[A, B](m1: M)(m2: M): M = m.combine(m1, m2)

  given optionMonad: Monad[Option] with
    def unit[A](a: => A): Option[A] = Some(a)
    extension [A](oa: Option[A])
      override def flatMap[B](f: A => Option[B]) = oa.flatMap(f)

  given eitherMonad[E]: Monad[Either[E, _]] with
    def unit[A](a: => A): Either[E, A] = ???
    extension [A](eea: Either[E, A])
      override def flatMap[B](f: A => Either[E, B]) = ???

  given stateMonad[S]: Monad[State[S, _]] with
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    extension [A](st: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)

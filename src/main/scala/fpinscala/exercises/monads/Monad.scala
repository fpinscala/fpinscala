package fpinscala.exercises
package monads

import parsing.*
import testing.*
import parallelism.*
import state.*
import parallelism.Par.*

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

  extension [A, B](fab: F[(A, B)]) def distribute: (F[A], F[B]) =
    (fab.map(_(0)), fab.map(_(1)))

  extension [A, B](e: Either[F[A], F[B]]) def codistribute: F[Either[A, B]] =
    e match
      case Left(fa) => fa.map(Left(_))
      case Right(fb) => fb.map(Right(_))

object Functor:
  given listFunctor: Functor[List] with
    extension [A](as: List[A])
      def map[B](f: A => B): List[B] = as.map(f)

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join
    
    def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    ???

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    ???

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    ???

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    ???

  extension [A](fa: F[A])
    def flatMapViaCompose[B](f: A => F[B]): F[B] =
      ???

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    ???

  extension [A](ffa: F[F[A]]) def join: F[A] =
    ???

  extension [A](fa: F[A])
    def flatMapViaJoinAndMap[B](f: A => F[B]): F[B] =
      ???

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    ???

end Monad      

object Monad:
  given genMonad: Monad[Gen] with
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    extension [A](fa: Gen[A])
      override def flatMap[B](f: A => Gen[B]): Gen[B] =
        fa.flatMap(f)

  given parMonad: Monad[Par] with
    def unit[A](a: => A) = ???
    extension [A](fa: Par[A])
      override def flatMap[B](f: A => Par[B]): Par[B] =
        ???

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new:
    def unit[A](a: => A) = ???
    extension [A](fa: P[A])
      override def flatMap[B](f: A => P[B]): P[B] =
        ???

  given optionMonad: Monad[Option] with
    def unit[A](a: => A) = ???
    extension [A](fa: Option[A])
      override def flatMap[B](f: A => Option[B]) =
        ???

  given lazyListMonad: Monad[LazyList] with
    def unit[A](a: => A) = ???
    extension [A](fa: LazyList[A])
      override def flatMap[B](f: A => LazyList[B]) =
        ???

  given listMonad: Monad[List] with
    def unit[A](a: => A) = ???
    extension [A](fa: List[A])
      override def flatMap[B](f: A => List[B]) =
        ???

end Monad

case class Id[+A](value: A):
  def map[B](f: A => B): Id[B] =
    ???
  def flatMap[B](f: A => Id[B]): Id[B] =
    ???

object Id:
  given idMonad: Monad[Id] with
    def unit[A](a: => A) = ???
    extension [A](fa: Id[A])
      override def flatMap[B](f: A => Id[B]) =
        ???

opaque type Reader[-R, +A] = R => A

object Reader:

  given readerMonad[R]: Monad[Reader[R, _]] with
    def unit[A](a: => A): Reader[R, A] = ???
    extension [A](fa: Reader[R, A])
      override def flatMap[B](f: A => Reader[R, B]) =
        ???

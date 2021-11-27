package fpinscala
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
    fas.foldRight(unit(List[A]()))((fa, acc) => fa.map2(acc)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => f(a).map2(acc)(_ :: _))

  // For `List`, the `replicateM` function will generate a list of lists.
  // It will contain all the lists of length `n` with elements selected from the
  // input list.
  // For `Option`, it will generate either `Some` or `None` based on whether the
  // input is `Some` or `None`. The `Some` case will contain a list of length `n`
  // that repeats the element in the input `Option`.
  // The general meaning of `replicateM` is described very well by the
  // implementation `sequence(List.fill(n)(fa))`. It repeats the `fa` monadic value
  // `n` times and gathers the results in a single value, where the monad `M`
  // determines how values are actually combined.

  // Recursive version:
  def _replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    if n <= 0 then unit(List[A]()) else fa.map2(_replicateM(n - 1, fa))(_ :: _)

  // Using `sequence` and the `List.fill` function of the standard library:
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

  extension [A](fa: F[A]) def _flatMap[B](f: A => F[B]): F[B] =
    compose((_:Unit) => fa, f)(())

  extension [A](ffa: F[F[A]]) def join: F[A] =
    ffa.flatMap(identity)

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(List[A]()))((a, acc) =>
      compose(f, (b: Boolean) => if (b) unit(a).map2(acc)(_ :: _) else acc)(a))

end Monad      

object Monad:
  given genMonad: Monad[Gen] with
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    extension [A](fa: Gen[A])
      override def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen.flatMap(fa)(f)

  given parMonad: Monad[Par] with
    def unit[A](a: => A) = Par.unit(a)
    extension [A](fa: Par[A])
      override def flatMap[B](f: A => Par[B]): Par[B] =
        Par.flatMap(fa)(f)

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new:
    def unit[A](a: => A) = p.succeed(a)
    extension [A](fa: P[A])
      override def flatMap[B](f: A => P[B]): P[B] =
        p.flatMap(fa)(f)

  given optionMonad: Monad[Option] with
    def unit[A](a: => A) = Some(a)
    extension [A](fa: Option[A])
      override def flatMap[B](f: A => Option[B]) =
        fa.flatMap(f)

  given lazyListMonad: Monad[LazyList] with
    def unit[A](a: => A) = LazyList(a)
    extension [A](fa: LazyList[A])
      override def flatMap[B](f: A => LazyList[B]) =
        fa.flatMap(f)

  given listMonad: Monad[List] with
    def unit[A](a: => A) = List(a)
    extension [A](fa: List[A])
      override def flatMap[B](f: A => List[B]) =
        fa.flatMap(f)

  // Since `State` is a binary type constructor, we need to partially apply it
  // with the `S` type argument. Thus, it is not just one monad, but an entire
  // family of monads, one for each type `S`.
  given stateMonad[S]: Monad[State[S, *]] with
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    extension [A](fa: State[S, A])
      override def flatMap[B](f: A => State[S, B]) =
        State.flatMap(fa)(f)


  def getState[S]: State[S,S] = State(s => (s,s))
  def setState[S](s: S): State[S,Unit] = State(_ => ((),s))

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int,A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc,a) => for {
      xs <- acc
      n  <- getState
      _  <- setState(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse

end Monad

type Id[+A] = A

object Id:
  given idMonad: Monad[Id] with
    def unit[A](a: => A) = a
    extension [A](fa: Id[A])
      override def flatMap[B](f: A => Id[B]) =
        f(fa)

opaque type Reader[-R, +A] = R => A

object Reader:
  def ask[R]: Reader[R, R] = r => r

  given readerMonad[R]: Monad[Reader[R, *]] with
    def unit[A](a: => A): Reader[R, A] = _ => a
    extension [A](fa: Reader[R, A])
      override def flatMap[B](f: A => Reader[R, B]) =
      r => f(fa(r))(r)

  // The action of Reader's `flatMap` is to pass the `r` argument along to both the
  // outer Reader and also to the result of `f`, the inner Reader. Similar to how
  // `State` passes along a state, except that in `Reader` the "state" is read-only.

  // The meaning of `sequence` here is that if you have a list of functions, you can
  // turn it into a function that takes one argument and passes it to all the functions
  // in the list, returning a list of the results.

  // The meaning of `join` is simply to pass the same value as both arguments to a
  // binary function.

  // The meaning of `replicateM` is to apply the same function a number of times to
  // the same argument, returning a list of the results. Note that if this function
  // is _pure_, (which it should be), this can be exploited by only applying the
  // function once and replicating the result instead of calling the function many times.
  // This means the Reader monad can override replicateM to provide a very efficient
  // implementation.

package fpinscala.streamingio

import fpinscala.iomonad._

import language.higherKinds

/*
 * A context in which exceptions can be caught and
 * thrown.
 */
trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](a: F[A]): F[Either[Throwable,A]]
  def fail[A](t: Throwable): F[A]
}

object MonadCatch {
  implicit def task = new MonadCatch[Task] {
    def unit[A](a: => A): Task[A] = Task.unit(a)
    def flatMap[A,B](a: Task[A])(f: A => Task[B]): Task[B] = a flatMap f
    def attempt[A](a: Task[A]): Task[Either[Throwable,A]] = a.attempt
    def fail[A](err: Throwable): Task[A] = Task.fail(err)
  }
}

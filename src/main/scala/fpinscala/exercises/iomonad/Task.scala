package fpinscala.exercises.iomonad

import fpinscala.answers.parallelism.Nonblocking.*
import java.util.concurrent.ExecutorService
import scala.util.control.NonFatal

/*
 * `Task[A]` is an opaque type around `IO[Either[Throwable, A]]` which is
 * an opaque type around `Free[Par, Either[Throwable, A]]`, with some
 * convenience functions for handling exceptions.
 */
opaque type Task[A] = IO[Either[Throwable, A]]

object Task:

  extension [A](self: Task[A])
    def flatMap[B](f: A => Task[B]): Task[B] =
      IO.monad.flatMap(self) {
        case Left(e) => IO(Left(e))
        case Right(a) => f(a)
      }

    def map[B](f: A => B): Task[B] = flatMap(f andThen Task.now)

    /* 'Catches' exceptions in the given task and returns them as values. */
    def attempt: Task[Either[Throwable, A]] =
      IO.monad.map(self) {
        case Left(e) => Right(Left(e))
        case Right(a) => Right(Right(a))
      }

    def handle[B >: A](f: PartialFunction[Throwable, B]): Task[B] =
      attempt.flatMap {
        case Left(e) => f.lift(e).map(Task.now).getOrElse(Task.fail(e))
        case Right(a) => Task.now(a)
      }

    def or[B >: A](t2: Task[B]): Task[B] =
      IO.monad.flatMap(self) {
        case Left(e) => t2
        case a => IO(a)
      }

    def unsafeRunSync(es: ExecutorService): A = IO.unsafeRunSync(self)(es) match
      case Left(e) => throw e
      case Right(a) => a

    def unsafeAttemptRunSync(es: ExecutorService): Either[Throwable,A] =
      try IO.unsafeRunSync(self)(es) catch { case t: Throwable => Left(t) }

  def apply[A](a: => A): Task[A] = IO(catchNonFatal(a))

  def fail[A](e: Throwable): Task[A] = IO(Left(e))
  def now[A](a: A): Task[A] = IO.now(Right(a))

  def more[A](a: => Task[A]): Task[A] = now(()).flatMap(_ => a)

  def delay[A](a: => A): Task[A] = more(apply(a))

  def fork[A](a: => Task[A]): Task[A] =
    IO.par(Par.lazyUnit(())).flatMap(_ => a)

  def forkUnit[A](a: => A): Task[A] = fork(now(a))

  private def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a) catch { case NonFatal(e) => Left(e) }

  given monad: Monad[Task] with
    def unit[A](a: => A) = Task(a)
    extension [A](fa: Task[A])
      def flatMap[B](f: A => Task[B]): Task[B] =
        Task.flatMap(fa)(f)

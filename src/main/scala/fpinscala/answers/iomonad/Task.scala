package fpinscala.answers.iomonad

import fpinscala.answers.parallelism.Nonblocking.*
import java.util.concurrent.ExecutorService
import scala.util.{Try, Success, Failure}
import scala.util.control.NonFatal

/*
 * `Task[A]` is an opaque type around `IO[Try[A]]` which is
 * an opaque type around `Free[Par, Try[A]]`, with some
 * convenience functions for handling exceptions.
 */
opaque type Task[A] = IO[Try[A]]

object Task:

  extension [A](self: Task[A])
    def flatMap[B](f: A => Task[B]): Task[B] =
      IO.monad.flatMap(self) {
        case Failure(e) => IO(Failure(e))
        case Success(a) => f(a)
      }

    def map[B](f: A => B): Task[B] = flatMap(f andThen Task.now)

    /* 'Catches' exceptions in the given task and returns them as values. */
    def attempt: Task[Try[A]] =
      IO.monad.map(self) {
        case Failure(e) => Success(Failure(e))
        case Success(a) => Success(Success(a))
      }

    def handle[B >: A](f: PartialFunction[Throwable, B]): Task[B] =
      attempt.flatMap {
        case Failure(e) => f.lift(e).map(Task.now).getOrElse(Task.fail(e))
        case Success(a) => Task.now(a)
      }

    def or[B >: A](t2: Task[B]): Task[B] =
      IO.monad.flatMap(self) {
        case Failure(e) => t2
        case a => IO(a)
      }

    def unsafeRunSync(es: ExecutorService): A = IO.unsafeRunSync(self)(es).get

    def unsafeAttemptRunSync(es: ExecutorService): Try[A] =
      try IO.unsafeRunSync(self)(es) catch { case NonFatal(t) => Failure(t) }

  def apply[A](a: => A): Task[A] = IO(Try(a))

  def fail[A](e: Throwable): Task[A] = IO(Failure(e))
  def now[A](a: A): Task[A] = IO.now(Success(a))

  def more[A](a: => Task[A]): Task[A] = now(()).flatMap(_ => a)

  def delay[A](a: => A): Task[A] = more(apply(a))

  def fork[A](a: => Task[A]): Task[A] =
    IO.par(Par.lazyUnit(())).flatMap(_ => a)

  def forkUnit[A](a: => A): Task[A] = fork(now(a))

  given monad: Monad[Task] with
    def unit[A](a: => A) = Task(a)
    extension [A](fa: Task[A])
      def flatMap[B](f: A => Task[B]): Task[B] =
        Task.flatMap(fa)(f)

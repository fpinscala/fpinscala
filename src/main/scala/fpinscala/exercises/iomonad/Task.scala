package fpinscala.exercises.iomonad

import fpinscala.exercises.parallelism.Nonblocking.*
import java.util.concurrent.ExecutorService

/*
 * `Task[A]` is a wrapper around `Free[Par, Either[Throwable, A]]`, with some
 * convenience functions for handling exceptions.
 */
case class Task[A](get: IO[Either[Throwable, A]]) {

  def flatMap[B](f: A => Task[B]): Task[B] =
    Task(get.flatMap {
      case Left(e) => IO(Left(e))
      case Right(a) => f(a).get
    })

  def map[B](f: A => B): Task[B] = flatMap(f andThen (Task.now))

  /* 'Catches' exceptions in the given task and returns them as values. */
  def attempt: Task[Either[Throwable,A]] =
    Task(get map {
      case Left(e) => Right(Left(e))
      case Right(a) => Right(Right(a))
    })

  def handle[B>:A](f: PartialFunction[Throwable,B]): Task[B] =
    attempt flatMap {
      case Left(e) => f.lift(e) map (Task.now) getOrElse Task.fail(e)
      case Right(a) => Task.now(a)
    }

  def or[B>:A](t2: Task[B]): Task[B] =
    Task(this.get flatMap {
      case Left(e) => t2.get
      case a => IO(a)
    })

  def run(implicit E: ExecutorService): A = unsafePerformIO(get) match {
    case Left(e) => throw e
    case Right(a) => a
  }

  def attemptRun(implicit E: ExecutorService): Either[Throwable,A] =
    try unsafePerformIO(get) catch { case t: Throwable => Left(t) }
}

object Task extends Monad[Task] {
  def unit[A](a: => A) = Task(IO(Try(a)))

  def flatMap[A,B](a: Task[A])(f: A => Task[B]): Task[B] =
    a.flatMap(f)

  def fail[A](e: Throwable): Task[A] = Task(IO(Left(e)))
  def now[A](a: A): Task[A] = Task(Return(Right(a)))

  def more[A](a: => Task[A]): Task[A] = Task.now(()).flatMap(_ => a)

  def delay[A](a: => A): Task[A] = more(now(a))
  def fork[A](a: => Task[A]): Task[A] =
    Task { par { Par.lazyUnit(()) }.flatMap(_ => a.get) }
  def forkUnit[A](a: => A): Task[A] = fork(now(a))

  def Try[A](a: => A): Either[Throwable,A] =
    try Right(a) catch { case e: Throwable => Left(e) }
}

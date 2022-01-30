package fpinscala.answers.iomonad

import fpinscala.answers.parallelism.Nonblocking.Par
import java.util.concurrent.{Executors, ExecutorService}

// This file contains the final form of IO that's developed throughout the chapter.
// See DerivingIO.scala for the step by step creation of IO.

opaque type IO[A] = Free[Par, A]

object IO:
  def now[A](a: A): IO[A] = Free.Return(a)

  def par[A](pa: Par[A]): IO[A] = Free.Suspend(pa)

  // Provides the syntax `IO { ...}` for creating synchronous IO blocks.
  def apply[A](a: => A): IO[A] = 
    par(Par.delay(a))

  // Provides the syntax `IO.async { cb => ... }` for creating asynchronous IO blocks.
  def async[A](cb: (A => Unit) => Unit): IO[A] =
    fork(par(Par.async(cb)))

  def fork[A](a: => IO[A]): IO[A] = par(Par.lazyUnit(())).flatMap(_ => a)

  def forkUnit[A](a: => A): IO[A] = fork(now(a))


  extension [A](ioa: IO[A])
    def unsafeRunSync(pool: ExecutorService): A =
      ioa.run.run(pool)

  given monad: Monad[IO] with
    def unit[A](a: => A) = IO(a)
    extension [A](fa: IO[A])
      def flatMap[B](f: A => IO[B]): IO[B] =
        fa.flatMap(f)

trait IOApp:

  def main(args: Array[String]): Unit =
    val pool = Executors.newFixedThreadPool(8)
    pureMain(args.toList).unsafeRunSync(pool)

  def pureMain(args: List[String]): IO[Unit]

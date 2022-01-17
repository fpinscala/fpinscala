package fpinscala.answers.iomonad

import fpinscala.answers.parallelism.Nonblocking.*
import IO3.given

type IO[A] = IO3.IO[A]
def IO[A](a: => A): IO[A] = IO3.IO[A](a)

def now[A](a: A): IO[A] = IO3.Free.Return(a)

def fork[A](a: => IO[A]): IO[A] = par(Par.lazyUnit(())).flatMap(_ => a)

def forkUnit[A](a: => A): IO[A] = fork(now(a))

def delay[A](a: => A): IO[A] = now(()).flatMap(_ => now(a))

def par[A](a: Par[A]): IO[A] = IO3.Free.Suspend(a)

def async[A](cb: ((A => Unit) => Unit)): IO[A] =
  fork(par(Par.async(cb)))

type Free[F[_], A] = IO3.Free[F, A]

def Return[A](a: A): IO[A] = IO3.Free.Return[Par,A](a)

// To run an `IO`, we need an executor service.
// The name we have chosen for this method, `unsafeRunSync`,
// reflects that is is unsafe, i.e. that it has side effects,
// and that it _performs_ the actual I/O.
import java.util.concurrent.ExecutorService
extension [A](ioa: IO[A])
  def unsafeRunSync(using e: ExecutorService): A =
    ioa.run.run(e)

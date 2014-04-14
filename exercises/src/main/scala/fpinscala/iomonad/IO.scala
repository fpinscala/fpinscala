package fpinscala.iomonad

object Exercises {
  /*
  We generalize `TailRec` to free monads.
  */

  sealed trait Free[F[_], A] {
    def map[B](f: A => B): Free[F,B] =
      flatMap(a => Return(f(a)))
    def flatMap[B](f: A => Free[F,B]): Free[F,B] = this match {
      case FlatMap(x, g) => FlatMap(x, (a: Any) => g(a).flatMap(f))
      case x => FlatMap(x, f)
    }
  }
  case class Return[F[_],A](a: A) extends Free[F, A]
  case class Suspend[F[_],A](s: F[Free[F, A]]) extends Free[F, A]
  case class FlatMap[F[_],A,B](s: Free[F, A],
                               f: A => Free[F, B]) extends Free[F, B]

  // Exercise 1: Implement the free monad
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = ???

  /*
  A data type that models only console I/O
  */
  sealed trait Console[R] {
    def map[S](f: R => S): Console[S] = this match {
      case ReadLine(k) => ReadLine(k andThen f)
      case PrintLine(s, k) => PrintLine(s, () => f(k()))
    }
  }

  case class ReadLine[R](k: Option[String] => R)
    extends Console[R]

  case class PrintLine[R](s: String, k: () => R)
    extends Console[R]

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] =
      Suspend(ReadLine((s: Option[String]) => Return(s)))

    def printLn(s: String): ConsoleIO[Unit] =
      Suspend(PrintLine(s, () => Return(())))
  }

  import Console._

  // Exercise 2: Implement `runConsole`
  def runConsole_[A](io: ConsoleIO[A]): A = ???


  // A basic state monad
  case class State[S,A](runState: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State { s =>
        val (a, s1) = runState(s)
        (f(a), s1)
      }
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State { s =>
        val (a, s1) = runState(s)
        f(a).runState(s1)
      }
  }

  implicit def stateMonad[S]: Monad[({type f[a] = State[S, a]})#f] =
    new Monad[({type f[a] = State[S, a]})#f] {
      def unit[A](a: => A) = State(s => (a, s))
      def flatMap[A,B](sa: State[S, A])(f: A => State[S, B]) = sa flatMap f
    }

  case class Buffers(in: List[String], out: List[String])

  // Exercise 3: Implement `runConsoleS`
  def runConsoleS_[A](io: ConsoleIO[A]): State[Buffers, A] = ???

  // A basic Reader Monad
  case class Reader[R, A](runReader: R => A) {
    def map[B](f: A => B): Reader[R, B] =
      Reader(r => f(runReader(r)))
    def flatMap[B](f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(runReader(r)).runReader(r))
  }

  implicit def readerMonad[R]: Monad[({type f[x] = Reader[R, x]})#f] =
    new Monad[({type f[x] = Reader[R, x]})#f] {
      def unit[A](a: => A) = Reader(_ => a)
      def flatMap[A,B](ra: Reader[R, A])(f: A => Reader[R, B]) = ra flatMap f
    }

  // Exercise 4: Write `runConsoleR`
  // Notice that this implementation is exactly like runConsoleS.
  // Only the types are different.
  def runConsoleR_[A](io: ConsoleIO[A]): Reader[String, A] = ???

  trait ~>[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  // Exercise 5: Write `runFree`
  // This shares a lot in common with `runConsoleR` and `runConsoleS`
  def runFree[F[_],G[_],A](free: Free[F,A])(t: F ~> G)
    (implicit G: Monad[G]): G[A] = ???


  type Id[A] = A

  implicit object IdMonad extends Monad[Id] {
    def unit[A](a: => A) = a
    def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  // Exercise 6: Write `runConsole` et al in terms of `runFree`:
  def runConsole[A](io: ConsoleIO[A]): A = ???

  def runConsoleR[A](io: ConsoleIO[A]): Reader[String, A] = ???

  def runConsoleS[A](io: ConsoleIO[A]): State[Buffers, A] = ???

  import fpinscala.parallelism.Nonblocking._

  // We conclude that a good representation of an `IO` monad is this:
  type IO[A] = Free[Par, A]

  object RunConsole extends (Console ~> Par) {
    def apply[A](c: Console[A]): Par[A] =
      c match {
        case ReadLine(k) => Par.lazyUnit {
          k(Option(readLine))
        }
        case PrintLine(a, k) => Par.lazyUnit {
          println(a)
          k()
        }
     }
  }

  implicit def parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A) = Par.lazyUnit(a)
    def flatMap[A,B](a: Par[A])(f: A => Par[B]) = Par.flatMap(a)(f)
  }

  def runConsoleNB[A](c: ConsoleIO[A]): Par[A] =
    runFree(c)(RunConsole)

  // To evaluate a `Par`, we need an `ExecutorService`
  import java.util.concurrent.ExecutorService
  def runPar(E: ExecutorService): Par ~> Id = new (Par ~> Id) {
    def apply[A](p: Par[A]) = Par.run(E)(p)
  }

  /*
   * Exercise 6: Implement a non-blocking read from an asynchronous file channel.
   */

  import java.nio._
  import java.nio.channels._

  // Provides the `Async { k => ... }` syntax for asyncronous IO blocks.
  def Async[A](cb: (A => Unit) => Unit): IO[A] =
    Suspend(Par.async((k: IO[A] => Unit) => cb(a => k(Return[Par,A](a)))))

  // Provides the `IO { ... }` syntax for synchronous IO blocks.
  def IO[A](a: => A): IO[A] = Suspend[Par,A](Par.delay(Return(a)))

  def read(file: AsynchronousFileChannel,
           fromPosition: Long,
           nBytes: Int): IO[Either[Throwable, Array[Byte]]] = ???

  // Exercise 7: Write Functor[Console] and Functor[Function0]
  val consoleFunctor: Functor[Console] = ???
  val function0Functor: Functor[Function0] = ???


  sealed trait FreeFunctor[F[_],A] {
    def map[B](f: A => B): FreeFunctor[F,B]
  }
  case class Map[F[_],I,A](fa: F[I], g: I => A) extends FreeFunctor[F,A] {
    def map[B](f: A => B) = Map(fa, g andThen f)
  }

  type FreeC[F[_],A] = Free[({type f[x] = FreeFunctor[F,x]})#f, A]

  implicit def request[F[_],A](fa: F[A]): FreeC[F,A] =
    Suspend[({type f[x] = FreeFunctor[F,x]})#f, A](Map(fa, (a: A) =>
      Return[({type f[x] = FreeFunctor[F,x]})#f, A](a)))

  // Exercise 7: Implement `freeLift`
  implicit def freeLift[F[_],G[_]](fg: F ~> G)(implicit G: Functor[G])
    : ({type f[x]=FreeFunctor[F,x]})#f ~> G = ???
}


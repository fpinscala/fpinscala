package fpinscala.answers.iomonad

import scala.io.StdIn.readLine
import scala.util.Try

object IO0:
                            /*

  Our first attempt at data type for representing computations that
  may perform I/O. Has a simple 'interpreter' baked in--the `run`
  function, which just returns `Unit`.

                             */
  trait IO:
    self =>
    def unsafeRun: Unit
    def ++(io: IO): IO = new:
      def unsafeRun =
        self.unsafeRun
        io.unsafeRun

  object IO:
    def empty: IO = new:
      def unsafeRun = ()

                            /*

  The API of this `IO` type isn't very useful.  Not many operations
  (it is only a monoid), and not many laws to help with reasoning. It
  is completely _opaque_. Also cannot represent _input_ effects, like
  reading from console, for instance:

                             */

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  // Ordinary code with side effects
  def converter: Unit =
    println("Enter a temperature in degrees Fahrenheit: ")
    val d = readLine().toDouble
    println(fahrenheitToCelsius(d))

  // A pure version is not possible!
  /*
  def converter: IO = {
    val prompt: IO = PrintLine("Enter a temperature in degrees fahrenheit: ")
    // now what ???
  }
  */
end IO0

object IO1:
                            /*

  We need a way for our `IO` actions to yield a result of some
  meaningful type. We do this by adding a type parameter to `IO`,
  which now forms a `Monad`.
                             */

  sealed trait IO[A]:
    self =>
    def unsafeRun: A
    def map[B](f: A => B): IO[B] = new:
      def unsafeRun = f(self.unsafeRun)
    def flatMap[B](f: A => IO[B]): IO[B] = new:
      def unsafeRun = f(self.unsafeRun).unsafeRun

  object IO:
    def apply[A](a: => A): IO[A] = new: // syntax for IO { .. }
      def unsafeRun = a

    given monad: Monad[IO] with
      def unit[A](a: => A): IO[A] = IO(a)
      extension [A](fa: IO[A])
        def flatMap[B](f: A => IO[B]) = fa.flatMap(f)

    def ref[A](a: A): IO[IORef[A]] = IO(new IORef(a))
    final class IORef[A](private var value: A):
      def set(a: A): IO[A] = IO { value = a; a }
      def get: IO[A] = IO(value)
      def modify(f: A => A): IO[A] = get.flatMap(a => set(f(a)))

  // We can now express the example

  def ReadLine: IO[String] = IO(readLine())
  def PrintLine(msg: String): IO[Unit] = IO(println(msg))
  import IO0.fahrenheitToCelsius

  def converter: IO[Unit] = for
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  yield ()

  /*                         Some other examples                      */

  // An `IO[Unit]` that reads a line from the console and echoes it back.
  val echo = ReadLine.flatMap(PrintLine)

  // Parses an `Int` by reading a line from the console.
  val readInt: IO[Int] = ReadLine.map(_.toInt)

  // Parses an `(Int,Int)` by reading two lines from the console.
  val readInts: IO[(Int,Int)] = readInt ** readInt

  // Repeat `converter` 5 times, discarding the results (which are
  // just `Unit`). We can replace `converter` here with any `IO`
  // action we wished to repeat 5 times (ex: `echo` or `readInts`).
  val prompts: IO[Unit] = converter.replicateM_(5)

  // An `IO[List[String]]` that will read 10 lines from the console and
  // return the list of results.
  val lines: IO[List[String]] = ReadLine.replicateM(10)

                            /*

  Larger example using various monadic combinators. Sample run:

     The Amazing Factorial REPL, v2.0
     q - quit
     <number> - compute the factorial of the given number
     <anything else> - bomb with horrible error
     3
     factorial: 6
     7
     factorial: 5040
     q

                             */
  val helpstring = """
  | The Amazing Factorial REPL, v2.0
  | q - quit
  | <number> - compute the factorial of the given number
  | <anything else> - bomb with horrible error
  """.trim.stripMargin

  // import all the combinators that come from `Monad`
  import IO.monad.* 

  def factorial(n: Int): IO[Int] =
    for
      acc <- IO.ref(1)
      _ <- foreachM(1 to n to(LazyList)) (i => acc.modify(_ * i).void)
      result <- acc.get
    yield result

  val factorialREPL: IO[Unit] = sequence_(
    PrintLine(helpstring),
    ReadLine.doWhile { line =>
      val ok = line != "q"
      when(ok) {
        for
          n <- factorial(line.toInt)
          _ <- PrintLine("factorial: " + n)
        yield ()
      }
    }
  )
end IO1

object IO2a:

  /*
  The previous IO representation overflows the stack for some programs.
  The problem is that `run` call itself recursively, which means that
  an infinite or long running IO computation will have a chain of regular
  calls to `run`, eventually overflowing the stack.

  The general solution is to make the `IO` type into a data type that we
  interpret using a tail recursive loop, using pattern matching.
  */

  enum IO[A]:
    case Return(a: A)
    case Suspend(resume: () => A)
    case FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f) // we do not interpret the `flatMap` here, just return it as a value
    def map[B](f: A => B): IO[B] =
      flatMap(a => Return(f(a)))

    // There is only one sensible way to implement this as a
    // tail-recursive function, the one tricky case is left-nested
    // flatMaps, as in `a.flatMap(f).flatMap(g)`, which we
    // reassociate to the right as `a.flatMap(ar => f(a).flatMap(g))`
    @annotation.tailrec final def unsafeRun(): A = this match
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match
        case Return(a) => f(a).unsafeRun()
        case Suspend(r) => f(r()).unsafeRun()
        case FlatMap(y, g) => y.flatMap(a => g(a).flatMap(f)).unsafeRun()

  object IO: // Notice that none of these operations DO anything
    def apply[A](a: => A): IO[A] =
      suspend(Return(a))

    def suspend[A](ioa: => IO[A]): IO[A] =
      Suspend(() => ioa).flatMap(identity)

    given monad: Monad[IO] with
      def unit[A](a: => A): IO[A] = IO(a)
      extension [A](fa: IO[A])
        def flatMap[B](f: A => IO[B]): IO[B] = fa.flatMap(f)

  def printLine(s: String): IO[Unit] =
    IO.Suspend(() => println(s))

  import IO.monad.*

  val p = printLine("Still going...").forever

  val actions: LazyList[IO[Unit]] =
    LazyList.fill(100000)(printLine("Still going..."))

  val composite: IO[Unit] =
    actions.foldLeft(IO(()))((acc, a) => acc.flatMap(_ => a))

end IO2a

object IO2aTests:
  import IO2a.*

  val f = (i: Int) => IO.Return(i)

  val g = List.fill(10000)(f).foldLeft(f)((a, b) => x => IO.suspend(a(x).flatMap(b)))

  @main def runIO2a: Unit =
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + gFortyTwo.unsafeRun())

end IO2aTests


object IO2b:

  /*
   * As it turns out, there's nothing about this data type that is specific
   * to I/O, it's just a general purpose data type for optimizing tail calls.
   * Here it is, renamed to `TailRec`. This type is also sometimes called
   * `Trampoline`, because of the way interpreting it bounces back and forth
   * between the main `run` loop and the functions contained in the `TailRec`.
   */

  enum TailRec[A]:
    case Return(a: A)
    case Suspend(resume: () => A)
    case FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)

    def map[B](f: A => B): TailRec[B] =
      flatMap(a => Return(f(a)))

    @annotation.tailrec final def run: A = this match
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match
        case Return(a) => f(a).run
        case Suspend(r) => f(r()).run
        case FlatMap(y, g) => y.flatMap(a => g(a).flatMap(f)).run

  object TailRec:
    def apply[A](a: => A): TailRec[A] =
      suspend(Return(a))

    def suspend[A](a: => TailRec[A]) =
      Suspend(() => a).flatMap(identity)

    given monad: Monad[TailRec] with
      def unit[A](a: => A): TailRec[A] = TailRec(a)
      extension [A](fa: TailRec[A])
        def flatMap[B](f: A => TailRec[B]): TailRec[B] = fa.flatMap(f)

end IO2b

object IO2bTests:
  import IO2b.*

  val f: Int => TailRec[Int] = (i: Int) => TailRec.Return(i)

  val g: Int => TailRec[Int] =
    List.fill(10000)(f).foldLeft(f)((a, b) => x => TailRec.suspend(a(x).flatMap(b)))

  @main def runIO2b: Unit =
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + gFortyTwo.run)

end IO2bTests

object BuiltInTailCalls:
  import scala.util.control.TailCalls.*

  val f: Int => TailRec[Int] = (i: Int) => done(i)

  val g: Int => TailRec[Int] =
    List.fill(10000)(f).foldLeft(f)((a, b) => x => tailcall(a(x).flatMap(b)))

  @main def runBuiltInTailCalls: Unit =
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + gFortyTwo.result)

end BuiltInTailCalls

object IO2c:

  import fpinscala.answers.parallelism.Nonblocking.*

  /*
   * We've solved our first problem of ensuring stack safety, but we're still
   * being very inexplicit about what sort of effects can occur, and we also
   * haven't found a way of describing asynchronous computations. Our `Suspend`
   * thunks will just block the current thread when run by the interpreter.
   * We could fix that by changing the signature of `Suspend` to take a `Par`.
   * We'll call this new type `Async`.
   */

  enum Async[A]:
    case Return(a: A)
    case Suspend(resume: Par[A]) // notice this is a `Par`
    case FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

    def flatMap[B](f: A => Async[B]): Async[B] =
      FlatMap(this, f)

    def map[B](f: A => B): Async[B] =
      flatMap(a => Return(f(a)))

    // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
    @annotation.tailrec final def step: Async[A] = this match
      case FlatMap(FlatMap(x, f), g) => x.flatMap(a => f(a).flatMap(g)).step
      case FlatMap(Return(x), f) => f(x).step
      case _ => this

    def run: Par[A] = step match
      case Return(a) => Par.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) => x match
        case Suspend(r) => r.flatMap(a => f(a).run)
        case _ => sys.error("Impossible, since `step` eliminates these cases")
    // The fact that `run` only uses the `unit` and `flatMap` functions of
    // `Par` is a clue that choosing `Par` was too specific of a choice,
    // this interpreter could be generalized to work with any monad.

  object Async:
    def apply[A](a: => A): Async[A] =
      suspend(Return(a))

    def suspend[A](aa: Async[A]): Async[A] =
      Suspend(Par.delay(aa)).flatMap(identity)

    given monad: Monad[Async] with
      def unit[A](a: => A): Async[A] = Async(a)
      extension [A](fa: Async[A])
        def flatMap[B](f: A => Async[B]): Async[B] = fa.flatMap(f)

end IO2c

object IO3:

  /*
  We can generalize `TailRec` and `Async` to the type `Free`, which is
  a `Monad` for any choice of `F`.
  */

  enum Free[F[_], A]:
    case Return(a: A)
    case Suspend(s: F[A])
    case FlatMap[F[_], A, B](
      s: Free[F, A],
      f: A => Free[F, B]) extends Free[F, B]

    def flatMap[B](f: A => Free[F,B]): Free[F,B] =
      FlatMap(this, f)

    def map[B](f: A => B): Free[F,B] =
      flatMap(a => Return(f(a)))

    // Exercise 3: Implement a `Free` interpreter which works for any `Monad`
    def run(using F: Monad[F]): F[A] = step match
      case Return(a) => F.unit(a)
      case Suspend(fa) => fa
      case FlatMap(Suspend(fa), f) => fa.flatMap(a => f(a).run)
      case FlatMap(_, _) => sys.error("Impossible, since `step` eliminates these cases")

    // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
    @annotation.tailrec
    final def step: Free[F, A] = this match
      case FlatMap(FlatMap(fx, f), g) => fx.flatMap(x => f(x).flatMap(y => g(y))).step
      case FlatMap(Return(x), f) => f(x).step
      case _ => this

    def runFree[G[_]](t: [x] => F[x] => G[x])(using G: Monad[G]): G[A] =
      step match
        case Return(a) => G.unit(a)
        case Suspend(r) => t(r)
        case FlatMap(Suspend(r), f) => t(r).flatMap(a => f(a).runFree(t))
        case FlatMap(_, _) => sys.error("Impossible, since `step` eliminates these cases")

    def translate[G[_]](fToG: [x] => F[x] => G[x]): Free[G, A] =
      runFree([x] => (fx: F[x]) => Suspend(fToG(fx)))

  import Free.{Return, Suspend, FlatMap}

  object Free:

    given freeMonad[F[_]]: Monad[[x] =>> Free[F, x]] with
      def unit[A](a: => A) = Return(a)
      extension [A](fa: Free[F, A])
        def flatMap[B](f: A => Free[F, B]) = fa.flatMap(f)

    extension [A](fa: Free[Function0, A])
      @annotation.tailrec
      def runTrampoline: A = fa match
        case Return(a) => a
        case Suspend(ta) => ta()
        case FlatMap(fx, f) => fx match
          case Return(x) => f(x).runTrampoline
          case Suspend(tx) => f(tx()).runTrampoline
          case FlatMap(fy, g) => fy.flatMap(y => g(y).flatMap(f)).runTrampoline

  /*
  The type constructor `F` lets us control the set of external requests our
  program is allowed to make. For instance, here is a type that allows for
  only console I/O effects.
  */

  import fpinscala.answers.parallelism.Nonblocking.Par

  enum Console[A]:
    case ReadLine extends Console[Option[String]]
    case PrintLine(line: String) extends Console[Unit]

    def toPar: Par[A] = this match
      case ReadLine => Par.lazyUnit(Try(readLine()).toOption)
      case PrintLine(line) => Par.lazyUnit(println(line))

    def toThunk: () => A = this match
      case ReadLine => () => Try(readLine()).toOption
      case PrintLine(line) => () => println(line)

    // other interpreters
    def toState: ConsoleState[A] = this match
      case ReadLine =>
        ConsoleState { bufs =>
          bufs.in match
            case List() => (None, bufs)
            case h :: t => (Some(h), bufs.copy(in = t))
        }
      case PrintLine(line) =>
        ConsoleState(bufs => ((), bufs.copy(out = bufs.out :+ line))) // append to the output

    def toReader: ConsoleReader[A] = this match
      case ReadLine =>
        ConsoleReader(in => Some(in))
      case PrintLine(line) =>
        ConsoleReader(s => ())

  object Console:
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] =
      Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] =
      Suspend(PrintLine(line))

  /*
  How do we actually _run_ a `ConsoleIO` program? We don't have a `Monad[Console]`
  for calling `run`, and we can't use `runTrampoline` either since we have `Console`,
  not `Function0`. We need a way to translate from `Console` to `Function0`
  (if we want to evaluate it sequentially) or a `Par`.
  */

  extension [A](fa: Free[Console, A])
    def toThunk: () => A =
      fa.runFree([x] => (c: Console[x]) => c.toThunk)

    def toPar: Par[A] =
      fa.runFree([x] => (c: Console[x]) => c.toPar)

  /*
  The `toThunk` implementation is unfortunately not stack safe,
  because it relies of the stack safety of the underlying monad, and the
  `Function0` monad we gave is not stack safe. To see the problem, try
  running: `freeMonad.forever(Console.printLn("Hello"))`.
  */

  extension [A](fa: Free[Console, A])
    def unsafeRunConsole: A =
      fa.translate([x] => (c: Console[x]) => c.toThunk).runTrampoline

  /*
  There is nothing about `Free[Console, A]` that requires we interpret
  `Console` using side effects. Here are two pure ways of interpreting
  a `Free[Console, A]`.
  */
  import Console.ConsoleIO

  // A specialized reader monad
  case class ConsoleReader[A](run: String => A):
    def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)))
    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)).run(r))

  object ConsoleReader:
    given monad: Monad[ConsoleReader] with
      def unit[A](a: => A) = ConsoleReader(_ => a)
      extension [A](fa: ConsoleReader[A])
        def flatMap[B](f: A => ConsoleReader[B]) = fa.flatMap(f)


  case class Buffers(in: List[String], out: Vector[String])

  // A specialized state monad
  case class ConsoleState[A](run: Buffers => (A, Buffers)):
    def map[B](f: A => B): ConsoleState[B] =
      ConsoleState { s =>
        val (a, s1) = run(s)
        (f(a), s1)
      }
    def flatMap[B](f: A => ConsoleState[B]): ConsoleState[B] =
      ConsoleState { s =>
        val (a, s1) = run(s)
        f(a).run(s1)
      }

  object ConsoleState:
    given monad: Monad[ConsoleState] with
      def unit[A](a: => A) = ConsoleState(bufs => (a,bufs))
      extension [A](fa: ConsoleState[A])
        def flatMap[B](f: A => ConsoleState[B]) = fa.flatMap(f)

  /* Can interpet these as before to convert our `ConsoleIO` to a pure value that does no I/O! */
  extension [A](fa: ConsoleIO[A])
    def toReader: ConsoleReader[A] =
      fa.runFree([x] => (c: Console[x]) => c.toReader)

    def toState: ConsoleState[A] =
      fa.runFree([x] => (c: Console[x]) => c.toState)

  // So `Free[F, A]` is not really an I/O type. The interpreter `runFree` gets
  // to choose how to interpret these `F` requests, and whether to do "real" I/O
  // or simply convert to some pure value!

  // NB: These interpretations are not stack safe for the same reason,
  // can instead work with `case class ConsoleReader[A](run: String => Trampoline[A])`,
  // which gives us a stack safe monad

  // We conclude that a good representation of an `IO` monad is this:
  type IO[A] = Free[Par, A]

  /*
   * Exercise 5: Implement a non-blocking read from an asynchronous file channel.
   * We'll just give the basic idea - here, we construct a `Future`
   * by reading from an `AsynchronousFileChannel`, a `java.nio` class
   * which supports asynchronous reads.
   */

  import java.nio.*
  import java.nio.channels.*

  // Provides the syntax `Async { k => ... }` for asyncronous IO blocks.
  def Async[A](cb: (A => Unit) => Unit): IO[A] =
    Suspend(Par.async(cb))

  // Provides the `IO { ... }` syntax for synchronous IO blocks.
  def IO[A](a: => A): IO[A] = Suspend(Par.delay(a))

  def read(file: AsynchronousFileChannel,
           fromPosition: Long,
           numBytes: Int): IO[Either[Throwable, Array[Byte]]] =
    Suspend(Par.async { (cb: Either[Throwable, Array[Byte]] => Unit) =>
      val buf = ByteBuffer.allocate(numBytes)
      file.read(buf, fromPosition, (), new CompletionHandler[Integer, Unit]:
        def completed(bytesRead: Integer, ignore: Unit) =
          val arr = new Array[Byte](bytesRead)
          buf.slice.get(arr, 0, bytesRead)
          cb(Right(arr))
        def failed(err: Throwable, ignore: Unit) =
          cb(Left(err))
      )
    })

  trait IOApp:
    import java.util.concurrent.{Executors, ExecutorService}

    extension [A](ioa: IO[A])
      def unsafeRunSync(pool: ExecutorService): A =
        ioa.run.run(pool)

    def main(args: Array[String]): Unit =
      val pool = Executors.newFixedThreadPool(8)
      pureMain(args.toList).unsafeRunSync(pool)

    def pureMain(args: List[String]): IO[Unit]

end IO3

object IO4:

  enum Free[+F[_], A]:
    case Return(a: A)
    case Suspend(s: F[A])
    case FlatMap[F[_], A, B](
      s: Free[F, A],
      f: A => Free[F, B]) extends Free[F, B]

    def flatMap[F2[x] >: F[x], B](f: A => Free[F2,B]): Free[F2,B] =
      FlatMap(this, f)

    def map[B](f: A => B): Free[F,B] =
      flatMap(a => Return(f(a)))

    def run[F2[x] >: F[x]](using F: Monad[F2]): F2[A] = step match
      case Return(a) => F.unit(a)
      case Suspend(fa) => fa
      case FlatMap(Suspend(fa), f) => fa.flatMap(a => f(a).run)
      case FlatMap(_, _) => sys.error("Impossible, since `step` eliminates these cases")

    @annotation.tailrec
    final def step: Free[F, A] = this match
      case FlatMap(FlatMap(fx, f), g) => fx.flatMap[F, A](x => f(x).flatMap[F, A](y => g(y))).step
      case FlatMap(Return(x), f) => f(x).step
      case _ => this

    def runFree[G[_]](t: [x] => F[x] => G[x])(using G: Monad[G]): G[A] =
      step match
        case Return(a) => G.unit(a)
        case Suspend(r) => t(r)
        case FlatMap(Suspend(r), f) => t(r).flatMap[A](a => (f(a): Free[F, A]).runFree[G](t))
        case FlatMap(_, _) => sys.error("Impossible, since `step` eliminates these cases")

    def translate[G[_]](fToG: [x] => F[x] => G[x]): Free[G, A] =
      runFree([x] => (fx: F[x]) => Suspend(fToG(fx)))


  object Free:
    given freeMonad[F[_]]: Monad[[x] =>> Free[F, x]] with
      def unit[A](a: => A) = Return(a)
      extension [A](fa: Free[F, A])
        def flatMap[B](f: A => Free[F, B]) = fa.flatMap(f)

    extension [A](fa: Free[Function0, A])
      @annotation.tailrec
      def runTrampoline: A = fa match
        case Return(a) => a
        case Suspend(ta) => ta()
        case FlatMap(fx, f) => fx match
          case Return(x) => f(x).runTrampoline
          case Suspend(tx) => f(tx()).runTrampoline
          case FlatMap(fy, g) => fy.flatMap(y => g(y).flatMap(f)).runTrampoline

  enum Console[A]:
    case ReadLine extends Console[Option[String]]
    case PrintLine(line: String) extends Console[Unit]

    def toThunk: () => A = this match
      case ReadLine => () => Try(readLine()).toOption
      case PrintLine(line) => () => println(line)

  object Console:
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] =
      Free.Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] =
      Free.Suspend(PrintLine(line))

  enum Files[A]:
    case ReadLines(file: String) extends Files[List[String]]
    case WriteLines(file: String, lines: List[String]) extends Files[Unit]

    def toThunk: () => A = this match
      case ReadLines(file) => () => List("line 1", "line 2")
      case WriteLines(file, lines) => () => ()

  object Files:
    type FilesIO[A] = Free[Files, A]

    def readLines(file: String): FilesIO[List[String]] =
      Free.Suspend(Files.ReadLines(file))

  def cat(file: String): Free[[x] =>> Files[x] | Console[x], Unit] =
    Files.readLines(file).flatMap { lines =>
      Console.printLn(lines.mkString("\n"))
    }

  extension [A](fa: Free[[x] =>> Files[x] | Console[x], A])
    def toThunk: () => A = 
      fa.runFree([x] => (fx: Files[x] | Console[x]) => fx match
        case c: Console[x] => c.toThunk
        case f: Files[x] => f.toThunk
      )

end IO4
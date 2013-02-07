package fpinscala.iomonad

object IO0 {
                            /*                       

  Our first attempt at data type for representing computations that 
  may perform I/O. Has a simple 'interpreter' baked in--the `run` 
  function, which just returns `Unit`.

                             */
  trait IO { self =>  
    def run: Unit 
    def ++(io: IO): IO = new IO {
      def run = { self.run; io.run } 
    }
  }
  object IO {
    def empty: IO = new IO { def run = () } 
  }

                            /* 

  The API of this `IO` type isn't very useful.  Not many operations 
  (it is only a monoid), and not many laws to help with reasoning. It 
  is completely _opaque_. Also cannot represent _input_ effects, like 
  reading from console, for instance:

                             */

  def fahrenheitToCelsius(f: Double): Double = 
    (f - 32) * 5.0/9.0

  // Ordinary code with side effects
  def converter: Unit = {
    println("Enter a temperature in degrees fahrenheit: ")
    val d = readLine.toDouble
    println(fahrenheitToCelsius(d))
  }
  
  // A pure version is not possible!
  /*
  def converter: IO = {
    val prompt: IO = PrintLine("Enter a temperature in degrees fahrenheit: ")
    // now what ???
  }
  */
}

object IO1 {
                            /* 

  We need a way for our `IO` actions to yield a result of some 
  meaningful type. We do this by adding a type parameter to `IO`, 
  which now forms a `Monad`.  
                             */

  trait IO[+A] { self => // give name for `this` pointer
    def run: A 
    def map[B](f: A => B): IO[B] = 
      new IO[B] { def run = f(self.run) } // `self` refs outer `IO` 
    def flatMap[B](f: A => IO[B]): IO[B] = 
      new IO[B] { def run = f(self.run).run }
  }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
    def apply[A](a: => A): IO[A] = unit(a) // syntax for IO { .. }

    def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO { value = a; a }
      def get: IO[A] = IO { value }
      def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
    }
  }

  // We can now express the example

  def ReadLine: IO[String] = IO { readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) } 
  import IO0.fahrenheitToCelsius
  
  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  /*                         Some other examples                      */

  import IO._ // import all the `IO` combinators that come from `Monad`

  // An `IO[Unit]` that reads a line from the console and echoes it back. 
  val echo = ReadLine.flatMap(PrintLine)

  // Parses an `Int` by reading a line from the console.  
  val readInt: IO[Int] = ReadLine.map(_.toInt)

  // Parses an `(Int,Int)` by reading two lines from the console. 
  val readInts: IO[(Int,Int)] = readInt ** readInt

  // Repeat `converter` 5 times, discarding the results (which are 
  // just `Unit`). We can replace `converter` here with any `IO` 
  // action we wished to repeat 5 times (ex: `echo` or `readInts`). 
  val prompts: IO[Unit] = replicateM_(5)(converter)

  // An `IO[List[String]]` that will read 10 lines from the console and 
  // return the list of results.
  val lines: IO[List[String]] = replicateM(10)(ReadLine)

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

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM (1 to n toStream) (i => acc.modify(_ * i).skip)
    result <- acc.get
  } yield result

  val factorialREPL: IO[Unit] = sequence_(
    IO { println(helpstring) },
    doWhile { IO { readLine } } { line => 
      val ok = line != "q"
      when (ok) { for { 
        n <- factorial(line.toInt) 
        _ <- IO { println("factorial: " + n) }
      } yield () }
    }  
  )
}

object IO2 {
                            /* 

  Our previous IO representation was quite inexplicit about where 
  interactions with the outside world were occurring. Let's make 
  that more explicit.
                             */
  trait IO[F[_], +A]
  case class Pure[F[_], +A](get: A) extends IO[F,A]
  case class Request[F[_], I, +A](
    expr: F[I], 
    receive: I => IO[F,A]) extends IO[F,A]

                            /* 

  Here's an example F that only allows access to the console. An
  `IO[Console, A]` is a computation yielding an `A` that may read
  and write to console.
                             */
  trait Console[A]
  case object ReadLine extends Console[Option[String]]
  case class PrintLine(s: String) extends Console[Unit]

                            /* 

  Nothing about `IO` _requires_ side effects. Any effects are a
  property of the _interpreter_ of `IO` values. Here's one way to
  encode the interpeter:

                             */
  trait Run[F[_]] { 
    def apply[A](expr: F[A]): (A, Run[F]) 
  }
  
  object IO {
    @annotation.tailrec
    def run[F[_],A](R: Run[F])(io: IO[F,A]): A = io match {
      case Pure(a) => a
      case Request(expr,recv) =>
        R(expr) match { case (e,r2) => run(r2)(recv(e)) }
    }
    
    def apply[A](a: => A): IO[Runnable,A] = 
      Request(Delay(a), (a:A) => Pure(a))
  }
  
                            /* 

  The interpreter can be built into the `F` type. 

                             */

  trait Runnable[A] { def run: A }
  object Delay { def apply[A](a: => A) = new Runnable[A] { def run = a } } 

                            /* 

  Two evaluators for Console, one which is pure, and one which 
  actually performs I/O. 
                             */

  object RunConsoleMock extends Run[Console] {
    def apply[A](c: Console[A]) = c match {
      case ReadLine => (Some("Hello world!"), RunConsoleMock)
      case PrintLine(_) => ((), RunConsoleMock) // Ignored! 
    }
  }
  
  object RunConsole extends Run[Console] {
    def apply[A](c: Console[A]) = c match {
      case ReadLine => 
        val r = try Some(readLine) catch { case _:Exception => None }
        (r, RunConsole)
      case PrintLine(s) => (println(s), RunConsole)
    }
  }
                            /* 

  Exercise 1: Implement the `Monad` for `IO`. Notice the
  implementation does not care about the `F` type parameter.

                             */

  def monad[F[_]] = new Monad[({ type f[a] = IO[F,a]})#f] {
    def unit[A](a: => A): IO[F,A] = Pure(a)
    def flatMap[A,B](a: IO[F,A])(f: A => IO[F,B]): IO[F,B] = 
      a match {
        case Pure(a) => f(a)
        case Request(expr,recv) => Request(expr, recv andThen (_ flatMap f)) 
      }
  }
                            /* 

  Exercise 2: Implement a `Run[Console]` which uses elements 
  from a `List[String]` to generate results from calls to 
  `ReadLine` (it can ignore `PrintLine` calls).

                             */
  def console(lines: List[String]): Run[Console] = new Run[Console] {
    def apply[A](c: Console[A]) = c match {
      case ReadLine => if (lines.isEmpty) (None, console(lines))  
                       else (Some(lines.head), console(lines.tail))
      case PrintLine(_) => ((), console(lines))
    }
  }
                            /* 

  Exercise 3: Implement `run` given a `Monad[F]`.

                             */
  def run[F[_],A](F: Monad[F])(io: IO[F,A]): F[A] = io match {
    case Pure(a) => F.unit(a)
    case Request(expr,recv) =>
      F.flatMap (expr) (e => run(F)(recv(e)))
  }

  
                            /* 

  Exercise 4: Try running these examples. For those that stack 
  overflow, can you explain why?

                             */

  val F = monad[Runnable]; import F._
  val ex1 = sequence_(Stream.fill(100000)(IO { math.random }))
  val ex2 = foreachM(Stream.range(1,100000))(i => IO { println(i) })
  val ex3 = forever(IO { println("hi") })
  val ex4 = Stream.fill(100000)("hi").foldLeft(IO { () })(
    (acc,s) => acc *> IO { println(s) })

                            /* 

  To illustrate the idea for how to fix this, we first introduce a
  data type, `Trampoline`:
                             */
  
  trait Trampoline[+A] { def run: A = Trampoline.run(this) }
  case class Done[+A](get: A) extends Trampoline[A]
  case class More[+A](force: () => Trampoline[A]) extends Trampoline[A]
  case class Bind[A,+B](force: () => Trampoline[A], 
                        f: A => Trampoline[B]) extends Trampoline[B]
 
                            /* 

  Exercise 5: Implement a tail-recursive `run` function for evaluating
  a `Trampoline[A]` to an `A`.
  
  Exercise 6: Implement `Monad[Trampoline]`. 

                             */

  // companion object itself is the Monad instance
  object Trampoline extends Monad[Trampoline] {
    @annotation.tailrec
    def run[A](t: Trampoline[A]): A = t match {
      case Done(a) => a
      case More(k) => run(k())
      case Bind(force, f) => run(force() flatMap f)
    }

    def unit[A](a: => A) = Done(a)
    def flatMap[A,B](a: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] = 
      a match { 
        case Done(forced) => f(forced)
        case More(force) => Bind(force, f)
        case Bind(force,g) => More(() => Bind(force, g andThen (_ flatMap f)))
      }
    def more[A](a: => Trampoline[A]): Trampoline[A] = 
      More(() => a)
    def delay[A](a: => A): Trampoline[A] = 
      More(() => Done(a))
  }
                            /* 

  Exercise 7: Show that `flatMap` always returns after doing a constant
  amount of work, and that `run` will always call itself after at most
  a single call to `force()`.
 
  First, we show that `flatMap` always does a constant amount of
  work before returning. Just by inspecting each of the cases, we
  can see that `flatMap` always returns 'immediately': 
    
    Bind(() => a, f) returns immediately, it is just packaging
      the arguments in a constructor
    Bind(force, f) returns immediately, for the same reason
    More(() => Bind(force, g andThen (_ flatMap f))) returns
      immediately. The `g andThen (_ flatMap f)` creates a new 
      function that could be problematic when called, but 
      composing the functions returns immediately.

  The next observation is that we never build up a chain of function
  compositions. Look at the third case: 

    case Bind(force,g) => More(() => Bind(force, g andThen (_ flatMap f)))

  Since this wraps its result in a `More`, if we call `flatMap` again,
  this will just become:

    Bind(() => Bind(force, g andThen (_ flatMap f)), f2)
  
  The same pattern continues if we flatMap again. Since we never 
  construct a function composition like f1 andThen f2 ... andThen fN, we
  don't have to worry about stack overflows when calling the function in 
  a Bind constructor.

  Next, notice that we never call force() anywhere in the implementation 
  of flatMap. The one place we construct a new thunk, it is a thunk that
  returns a Bind 'immediately', without forcing any other thunk:

     case Bind(force,g) => More(() => Bind(force, g andThen (_ flatMap f)))

  Since we never construct a thunk that forces another thunk, and any 
  thunk we do construct uses constant stack space to produce its result,
  we can be assured that calling `force()` in the `run` function will use
  a constant amount of stack space before returning. 

                             */
  
}

object IO3 {
                            /* 

  Using our newfound knowledge of trampolining, we can now bake 
  trampolining support into our `IO` type:

                             */
  
  trait IO[F[_], +A]

  object IO {
    // it is good practice to place the constructors for a data type
    // inside the companion object for the type, to avoid polluting
    // the namespace
    case class Pure[F[_], +A](get: A) extends IO[F,A]
    case class Request[F[_],I,+A](
        expr: F[I], 
        receive: I => IO[F,A]) extends IO[F,A]
    case class BindMore[F[_],A,+B](
        force: () => IO[F,A], 
        f: A => IO[F,B]) extends IO[F,B]
    case class BindRequest[F[_],I,A,+B](
        expr: F[I], receive: I => IO[F,A], 
        f: A => IO[F,B]) extends IO[F,B]
    case class More[F[_],A](force: () => IO[F,A]) extends IO[F,A]
  
                            /* 

  Exercise 8: Implement both versions of `run` for this new `IO` type.
  Exercise 9: Implement `Monad` for this new version of `IO`. 
  Exercise 10: Implement `run` given an arbitrary `Monad[F]`.

                             */
    import IO2.Run
    // enable monadic syntax for `IO` type, including infix `flatMap`
    implicit def toMonadic[F[_],A](a: IO[F,A]) = 
      monad[F].toMonadic(a)

    @annotation.tailrec
    def run[F[_],A](R: Run[F])(io: IO[F,A]): A = io match {
      case Pure(a) => a  
      case Request(expr,k) => 
        R(expr) match { case (e,r2) => run(r2)(k(e)) }
      case BindRequest(expr,k,f) => 
        R(expr) match { case (e,r2) => run(r2)(k(e) flatMap f) }
      case BindMore(k,f) => run(R)(k() flatMap f)
      case More(k) => run(R)(k())
    }
    
    def run[F[_],A](F: Monad[F])(io: IO[F,A]): F[A] = io match {
      case Pure(a) => F.unit(a)  
      case Request(expr,k) => 
        F.flatMap (expr) (e => run(F)(k(e)))
      case BindRequest(expr,k,f) => 
        F.flatMap (expr) (e => run(F)(k(e) flatMap f))
      case BindMore(k,f) => run(F)(k() flatMap f) 
      case More(k) => run(F)(k())
    }

    def monad[F[_]]: Monad[({ type f[x] = IO[F,x]})#f] = 
      new Monad[({ type f[x] = IO[F,x]})#f] {
        def unit[A](a: => A) = Pure(a)
        def flatMap[A,B](a: IO[F,A])(f: A => IO[F,B]): IO[F,B] = a match {
          case Pure(a) => f(a)
          case Request(expr,k) => BindRequest(expr,k,f)
          case More(k) => BindMore(k,f)
          // recall: 
          // def seq[A,B,C](f: A => F[B])(g: B => F[C]): A => F[C]
          case BindMore(k,g) => More(() => BindMore(k, seq(g)(f))) 
          case BindRequest(expr,k,g) => More(() => BindRequest(expr, k, seq(g)(f)))
        }
      }
     
    /* 
     * Convenience function for constructing `IO` actions, we pick
     * `Task` as the `F` parameter. See `Task.scala`.   
     */
    def apply[A](a: => A): IO[Task,A] = 
      Request(Task.delay(a), (a:A) => Pure(a))
    
    val RunTask: Run[Task] = new Run[Task] { 
      def apply[A](f: Task[A]) = (f.run, RunTask) 
    }

    // enable infix syntax for `run` and `runAsync` for IO[Future,A]
    implicit class IOTask[A](f: IO[Task,A]) {
      def run: A = IO.run(RunTask)(f)
      def runAsync: Task[A] = IO.run(Task)(f)
    }
  } 

                            /* 

  We are going to implement a nonblocking version of `run`, that returns 
  a `Future[A]`. First, we introduce a new, trampolined version of 
  `Future` (see constructors in companion object of `Future`):
                             */
  trait Future[+A] {
    
                            /* 

  Exercise 11: Implement `Monad[Future]`. We will need it to implement
  our nonblocking `IO` interpreter. Also implement `runAsync`, for an 
  asynchronous evaluator for `Future`, and `run`, the synchronous 
  evaluator.
                             */

    import Future._ // constructors and helper functions, see below

    def flatMap[B](f: A => Future[B]): Future[B] = this match {
      case Now(a) => More(() => f(a))
      case Later(listen) => BindLater(listen, f)
      case More(force) => BindMore(force, f)
      case BindLater(listen,g) => More(() =>
        BindLater(listen)(x => g(x) flatMap f))
      case BindMore(force,g) => More(() =>
        BindMore(force)(x => g(x) flatMap f))
    }
    def map[B](f: A => B): Future[B] =
      this flatMap (a => Now(f(a)))
   
    final def runAsync(onFinish: A => Unit): Future[Unit] = this.start match {
      case Now(a) => now(onFinish(a))
      case Later(r) => now(r(onFinish))
      case BindLater(r, f) =>
        val latch = new java.util.concurrent.CountDownLatch(1)
        @volatile var result: Option[Any] = None
        r { x => result = Some(x); latch.countDown }
        more { latch.await; f(result.get).runAsync(onFinish) }
      case _ => sys.error("Impossible!")
    }
   
    @annotation.tailrec
    final def start: Future[A] = this match {
      case More(force) => force().start
      case BindMore(force,f) => (force() flatMap f).start
      case _ => this // Now, Later, BindLater
    }
   
    // synchronous evaluator
    def run: A = this.start match {
      case Now(a) => a
      case f => {
        val latch = new java.util.concurrent.CountDownLatch(1)
        @volatile var result: Option[A] = None
        f.runAsync { a => result = Some(a); latch.countDown }.run
        latch.await
        result.get
      }
    }
  }
 
  object Future extends Monad[Future] {
    // define constructors inside companion object
    case class Now[+A](get: A) extends Future[A]
    case class More[+A](force: () => Future[A]) extends Future[A]
    case class BindMore[A,+B](force: () => Future[A],
                              f: A => Future[B]) extends Future[B]
   
    case class Later[+A](listen: (A => Unit) => Unit) extends Future[A]
    case class BindLater[A,+B](listen: (A => Unit) => Unit,
                               f: A => Future[B]) extends Future[B]
   
    // curried versions for type inference
    def BindLater[A,B](listen: (A => Unit) => Unit)(
                       f: A => Future[B]): Future[B] =
      BindLater(listen, f)
   
    def BindMore[A,B](force: () => Future[A])(
                      f: A => Future[B]): Future[B] =
      BindMore(force, f)
   
    // hide java.util.concurrent.Future, import everything else
    import java.util.concurrent.{Future => _, _}
    
    def unit[A](a: => A) = now(a)
    def flatMap[A,B](a: Future[A])(f: A => Future[B]): Future[B] = a flatMap f
    def more[A](a: => Future[A]): Future[A] = More(() => a)
    def delay[A](a: => A): Future[A] = More(() => Now(a))
    def now[A](a: A): Future[A] = Now(a)
    def fork[A](a: => Future[A]): Future[A] = apply(a) flatMap (a => a)
   
    // Create a Later from a nonstrict value,
    // backed by a shared thread pool (declared below)
    def apply[A](a: => A): Future[A] = {
      @volatile var result: Option[A] = None
      val latch = new CountDownLatch(1)
      val task = pool.submit { new Callable[Unit] {
        def call = { result = Some(a); latch.countDown }
      }}
      more { Later { cb => latch.await; cb(result.get) } }
    }
    // Daemon threads will not prevent the JVM from exiting, if they are
    // the only threads left running (see java.lang.Thread API docs for
    // details)
    val daemonize = new ThreadFactory { def newThread(r: Runnable) = {
      val t = new Thread(r)
      t.setDaemon(true)
      t
    }}
    val pool = Executors.newCachedThreadPool(daemonize)
  }
 
                            /* 

  With `Future`, our existing `Monad`-parameterized `run` function
  can use nonblocking I/O - we just need to be able to produce a 
  `Future` from a nonblocking I/O call, which we can do using the 
  `Later` constructor. However, what do we do if we have an 
  `IO[Console,A]`? We can translate this to an `IO[Future,A]` 
  incrementally while running the `IO` action - 

                             */
   
  import IO._

  trait Trans[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }
  
  /* 
   * Exercise 12 Implement `run`, translating from `F` to `G` 
   * as part of the interpretation.
   */
  def run[F[_],G[_],A](T: Trans[F,G])(G: Monad[G])(io: IO[F,A]): G[A] = 
    io match {
      case Pure(a) => G.unit(a)  
      case Request(expr,k) => 
        G.flatMap (T(expr)) (e => run(T)(G)(k(e)))
      case BindRequest(expr,k,f) => 
        G.flatMap (T(expr)) (e => run(T)(G)(k(e) flatMap f))
      case BindMore(k,f) => run(T)(G)(k() flatMap f) 
      case More(k) => run(T)(G)(k())
    }
                            /* 

  Exercise 13: We can also use an API that supports nonblocking operations
  directly. Left as an exercise; the general idea is to use the
  I/O API to construct a Later directly, rather that using Future.apply,
  which delegates to a thread pool.

                             */

                             /*
  // todo - check that this code actually works with Java 7
  import java.nio._
  import java.nio.channels._

  def read(file: AsynchronousFileChannel, 
           fromPosition: Long, 
           nBytes: Int): Future[Either[Throwable, Array[Byte]]] = {
    val buf = ByteBuffer.allocate(nBytes)
    Future.Later { (cb: Array[Byte] => Unit) => 
      file.read(buf, fromPosition, (), new CompletionHandler[Int, Unit] {
        def completed(bytesRead: Int, ignore: Unit) = {
          val arr = new Array[Byte](bytesRead)
          buf.slice.get(arr, 0, bytesRead)
          cb(Right(arr))
        }
        def failed(err: Throwable, ignore: Unit) = 
          cb(Left(err))
      })
    }
  }
                               */
     
  
}

object FactorialApp extends App {
  import IO1._
  factorialREPL.run
}

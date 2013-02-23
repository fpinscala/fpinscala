package fpinscala.iomonad

/** 
 * A trampolined computation producing an `A` that may include 
 * asynchronous steps. See constructors in companion object.
 */
trait Future[+A] {
  import Future._
                            /* 

  Exercise 11: Implement `Monad[Future]`. We will need it to implement
  our nonblocking `IO` interpreter. Also implement `runAsync`, for an 
  asynchronous evaluator for `Future`, and `run`, the synchronous 
  evaluator.
                             */

  def flatMap[B](f: A => Future[B]): Future[B] = this match {
    case Now(a) => f(a)
    case More(thunk) => BindMore(thunk, f)
    case Async(listen) => BindAsync(listen, f)
    case BindMore(thunk, g) => 
      More(() => BindMore(thunk, g andThen (_ flatMap f)))
    case BindAsync(listen, g) => 
      More(() => BindAsync(listen, g andThen (_ flatMap f))) 
  }

  def map[B](f: A => B): Future[B] =  
    flatMap(f andThen (b => Future.unit(b)))

  def listen(cb: A => Trampoline[Unit]): Unit = 
    this.step match {
      case Now(a) => cb(a) 
      case Async(onFinish) => onFinish(cb)
      case BindAsync(onFinish, g) => 
        onFinish(x => Trampoline.delay(g(x)) map (_ listen cb))
    }

  @annotation.tailrec
  final def step: Future[A] = this match {
    case More(thunk) => thunk().step
    case BindMore(thunk, f) => (thunk() flatMap f).step 
    case _ => this
  }

  def start: Future[A] = {
    val latch = new java.util.concurrent.CountDownLatch(1)
    @volatile var result: Option[A] = None
    runAsync { a => result = Some(a); latch.countDown } 
    delay { latch.await; result.get }
  }

  def runAsync(cb: A => Unit): Unit = 
    listen(a => Trampoline.done(cb(a)))

  def run: A = {
    val latch = new java.util.concurrent.CountDownLatch(1) 
    @volatile var result: Option[A] = None
    runAsync { a => result = Some(a); latch.countDown }
    latch.await
    result.get
  }
}

object Future extends Monad[Future] {

  case class Now[+A](a: A) extends Future[A]
  case class Async[+A](onFinish: (A => Trampoline[Unit]) => Unit) extends Future[A]
  case class More[+A](thunk: () => Future[A]) extends Future[A]
  case class BindMore[A,B](thunk: () => Future[A], f: A => Future[B]) extends Future[B]
  case class BindAsync[A,B](onFinish: (A => Trampoline[Unit]) => Unit,
                            f: A => Future[B]) extends Future[B]

  def unit[A](a: => A): Future[A] = apply(a) // more { Now(a) } 
  def now[A](a: A): Future[A] = Now(a)
  def delay[A](a: => A): Future[A] = More(() => Now(a))
  def fork[A](a: => Future[A]): Future[A] = apply(a) flatMap (a => a)

  def async[A](listen: (A => Unit) => Unit): Future[A] = 
    Async((cb: A => Trampoline[Unit]) => listen { a => cb(a).run })
  
  def more[A](f: => Future[A]): Future[A] = More(() => f)

  def flatMap[A,B](fa: Future[A])(f: A => Future[B]): Future[B] =   
    fa flatMap f

  import java.util.concurrent.{Callable, Executors, ThreadFactory}

  def apply[A](a: => A): Future[A] = Async { cb => 
    pool.submit { new Callable[Unit] { def call = cb(a).run }}
  }

  // Daemon threads will not prevent the JVM from exiting, if they are
  // the only threads left running (see java.lang.Thread API docs for
  // details)
  val daemonize = new ThreadFactory { def newThread(r: Runnable) = {
    val t = new Thread(r)
    t.setDaemon(true)
    t
  }}

  //val pool = Executors.newCachedThreadPool(daemonize)
  val pool = Executors.newFixedThreadPool(1, daemonize)

                            /* 

  Exercise 13: We can also use an API that supports nonblocking operations
  directly. Left as an exercise; the general idea is to use the
  I/O API to construct a Later directly, rather that using Future.apply,
  which delegates to a thread pool.

                             */

  /* 
   * We'll just give the basic idea - here, we construct a `Future`
   * by reading from an `AsynchronousFileChannel`, a `java.nio` class 
   * which supports asynchronous reads.  
   */

  import java.nio._
  import java.nio.channels._

  def read(file: AsynchronousFileChannel, 
           fromPosition: Long, 
           nBytes: Int): Future[Either[Throwable, Array[Byte]]] = {
    val buf = ByteBuffer.allocate(nBytes)
    Async { (cb: Either[Throwable,Array[Byte]] => Trampoline[Unit]) => 
      file.read(buf, fromPosition, (), new CompletionHandler[Integer, Unit] {
        def completed(bytesRead: Integer, ignore: Unit) = {
          val arr = new Array[Byte](bytesRead)
          buf.slice.get(arr, 0, bytesRead)
          cb(Right(arr)).run
        }
        def failed(err: Throwable, ignore: Unit) = 
          cb(Left(err)).run
      })
    }
  }
}

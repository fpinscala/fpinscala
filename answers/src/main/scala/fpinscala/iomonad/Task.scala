package fpinscala.iomonad

/* 
 * `Task[A]` is a `Future[Either[Throwable,A]]`, with some convenience 
 * functions for handling exceptions. Its `Monad` instance is derived
 * from the `Monad` instance for `Future`.
 */
case class Task[+A](get: Future[Either[Throwable,A]]) {
  
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
      case a => Future.now(a)
    })

  def run: A = get.run match {
    case Left(e) => throw e 
    case Right(a) => a
  }

  def attemptRun: Either[Throwable,A] = 
    try get.run catch { case t: Throwable => Left(t) }

  def runAsync: Future[Either[Throwable,A]] = get.start
}

object Task extends Monad[Task] {
  def unit[A](a: => A) = Task(Future.unit(Try(a))) 

  def flatMap[A,B](a: Task[A])(f: A => Task[B]): Task[B] = 
    Task(a.get.flatMap { 
      case Left(e) => Future.now(Left(e))
      case Right(a) => f(a).get
    }) 

  def fail(e: Throwable): Task[Nothing] = Task(Future.now(Left(e))) 
  def now[A](a: A): Task[A] = Task(Future.now(Right(a)))
  def more[A](a: => Task[A]): Task[A] = Task(Future.more(
    Try(a.get) match { 
      case Left(e) => Future.now(Left(e))
      case Right(f) => f
  }))

  def delay[A](a: => A): Task[A] = more(now(a))
  def fork[A](a: => A): Task[A] = Task(Future(Try(a)).start)
//  def async[A](register: (Either[Throwable,A] => Unit) => Unit): Task[A] = 
//   Task(Future.Later(register))

  def Try[A](a: => A): Either[Throwable,A] = 
    try Right(a) catch { case e: Exception => Left(e) }
}


// vim: set ts=4 sw=4 et:

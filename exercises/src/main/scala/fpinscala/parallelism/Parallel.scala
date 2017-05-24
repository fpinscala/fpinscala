package fpinscala.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


object Parallel {

  type Parallel[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true
    def isCancelled: Boolean = false

    def get(timeout: Long, units: TimeUnit): A = get
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Parallel[A] = _ => UnitFuture(a)

  def lazyUnit[A](a: A): Parallel[A] = fork(unit(a))

  def run[A](a: Parallel[A]): A = ???

  def fork[A](a: => Parallel[A]): Parallel[A] = es =>
    es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  /**
    * Exercise 7.1 - `Par.map2` is a new higher-order function for combining
    * the result of two parallel computations. What is its signature? Give
    * the most general signature possible.
    */
  def map2[A, B, C](a: Parallel[A], b: Parallel[B])(f: (A, B) => C): Parallel[C] = es => {
    val af: Future[A] = a(es)
    val bf: Future[B] = b(es)

    UnitFuture(f(af.get, bf.get))
  }

  /**
    * Exercise 7.2 - Before continuing, try to come up with representations for Par that make it possible to implement
    * the functions of our API.
    *
    * The `ExecutorService` class in the Java standard library already allows us to submit an element for computation in
    * a separate logical thread. It even returns a `Future[A]` which provides a simple API to specify waiting for the
    * future to complete. If we use these components, then our notion of `Parallel[A]` simply becomes an extension of
    * the `submit` method on `ExecutorService`, or `ExecutorService => Future[A]`.
    */


  /**
    * Exercise 7.3 - Hard: Fix the implementation of `map2` so that it
    * respects the contract of timeouts on Future.
    *
    * Note that this solution makes the design choice to treat time remaining as state associated with calling `get` on
    * a `Future` - this is abstracted out by creating a new implementation `TimedFuture[A]` which provides a new method
    * called `getWithRemaining` that returns a tuple consisting of the result, and remaining time and units.
    *
    * While this solution is elegant in its treatment of `get` as a stateful computation, it forces us to deal with the
    * specification of the timeout in our call to `map2`. The solution below just explicitly asserts it in the body of
    * the function, which clearly won't work in general. The options are to either push it to the front of the call to
    * `map2`, or change the signature of `map2` to accept a timeout in addition. Both of these options have drawbacks.
    *
    * The crucial insight to make here is that a) callers of `map2` are going to have to call `get` on its result at
    * some point anyway, if they want the contents, and that b) the necessary components of respecting timeouts requires
    * both the `Future[A]` yielded by `a` and the `Future[B]` yielded by `b`. This allows for a different implementation
    * of `Future[T]` that wraps both futures as well as the function `f` to apply to their results.
    *
    * See the `map2WithTimeoutsBookSolution` for details on the optimal approach.
    */
  def map2WithTimeouts[A, B, C](a: Parallel[A], b: Parallel[B])(f: (A, B) => C): Parallel[C] = es => {
    val af = TimedFuture(a(es))
    val bf = TimedFuture(b(es))

    // assume hard coded time
    val timeout = 30
    val units = TimeUnit.SECONDS

    val (aa, t1, u1) = af.getWithRemaining(timeout, units)
    val (bb, _, _) = bf.getWithRemaining(t1, u1)

    UnitFuture(f(aa, bb))
  }

  private case class TimedFuture[A](future: Future[A]) extends Future[A] {
    def getWithRemaining(timeout: Long, units: TimeUnit): (A, Long, TimeUnit) = {
      val millis = units.toMillis(timeout)
      val start = System.currentTimeMillis()
      val result = future.get(millis, TimeUnit.MILLISECONDS)

      (result, System.currentTimeMillis() - start, TimeUnit.MILLISECONDS)
    }

    override def isCancelled: Boolean =
      future.isCancelled

    override def get(): A =
      future.get

    override def get(timeout: Long, unit: TimeUnit): A =
      future.get(timeout, unit)

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      future.cancel(mayInterruptIfRunning)

    override def isDone: Boolean =
      future.isDone
  }

  def map2WithTimeoutsBookSolution[A, B, C](a: Parallel[A], b: Parallel[B])(f: (A, B) => C): Parallel[C] = es => {
    val af = a(es)
    val bf = b(es)

    Map2Future(af, bf, f)
  }

  private case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    override def isCancelled: Boolean =
      a.isCancelled || b.isCancelled

    override def isDone: Boolean =
      a.isDone && b.isDone

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

    override def get(): C =
      compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    private def compute(timeout: Long): C = {
      val start = System.nanoTime
      val aRes = a.get(timeout, TimeUnit.NANOSECONDS)
      val end = System.nanoTime
      val bRes = b.get(timeout - (end - start), TimeUnit.NANOSECONDS)

      f(aRes, bRes)
    }
  }

  /**
    * Exercise 7.4 - This API already enables a rich set of operations. Here’s a simple example: using `lazyUnit`, write
    * a function to convert any function `A => B` to one that evaluates its result asynchronously.
    */
  def asyncF[A, B](f: A => B): A => Parallel[B] =
    a => lazyUnit(f(a))


  def sortPar(parList: Parallel[List[Int]]): Parallel[List[Int]] =
    map(parList)(_.sorted)

  def map[A, B](a: Parallel[A])(f: A => B): Parallel[B] =
    map2(a, unit(()))((a, _) => f(a))

  /**
    * Exercise 7.5 - Hard: Write this function, called `sequence`. No additional primitives are required. Do not call
    * `run`.
    *
    * Note that this implementation is conceptually the easiest, and does not fork any of the individual processes onto
    * new logical threads. If we wanted to do that, we would have to call `fork` explicitly, which we do in the two
    * alternative implementations below.
    */
  def sequence[A](ps: List[Parallel[A]]): Parallel[List[A]] =
    ps.foldRight(unit(Nil: List[A]))((p, acc) => map2(p, acc)(_ :: _))

  /**
    * Recursively forks calls to new logical threads. Note that while this implementation is effectively tail-recursive,
    * we can achieve better performance by splitting the list in half and forking each half onto a new logical thread,
    * instead of recursively forking the tail of the input list.
    */
  def sequenceRight[A](ps: List[Parallel[A]]): Parallel[List[A]] = ps match {
    case Nil    => unit(Nil)
    case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
  }

  /**
    * Forks calls to new logical threads by recursively splitting the input in half. Note that we use `IndexedSeq` and
    * its default implementation `Vector` in order to have an efficient function for splitting the input in half. This
    * would not be quite the same with `List`.
    */
  def sequenceHalf[A](ps: IndexedSeq[Parallel[A]]): Parallel[IndexedSeq[A]] = fork {
    ps.length match {
      case 0 => unit(Vector())
      case 1 => map(ps.head)(Vector(_))
      case _ => val (l, r) = ps.splitAt(ps.length / 2) ; map2(sequenceHalf(l), sequenceHalf(r))(_ ++ _)
    }
  }

  /**
    * For convenience, we can provide the same API.
    */
  def sequenceFinal[A](ps: List[Parallel[A]]): Parallel[List[A]] =
    map(sequenceHalf(ps.toIndexedSeq))(_.toList)

  def parMap[A, B](as: List[A])(f: A => B): Parallel[List[B]] = fork {
    val ps = as map asyncF(f)
    sequenceFinal(ps)
  }

  /**
    * Exercise 7.6 - Implement `parFilter`, which filters elements of a list in parallel.
    */
  def parFilter[A](as: List[A])(f: A => Boolean): Parallel[List[A]] = fork {
    val pars = as.foldRight(Nil: List[Parallel[A]])((a, acc) => if (f(a)) lazyUnit(a) :: acc else acc)
    sequenceFinal(pars)
  }

  def parFilterBookSolution[A](as: List[A])(f: A => Boolean): Parallel[List[A]] = {
    val pars: List[Parallel[List[A]]] = as map asyncF(a => if (f(a)) List(a) else Nil)
    map(sequenceFinal(pars))(_.flatten)
  }
}

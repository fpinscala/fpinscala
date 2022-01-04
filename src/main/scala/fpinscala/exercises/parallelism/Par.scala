package fpinscala.exercises.parallelism

import java.util.concurrent.*

object Par:
  opaque type Par[A] = ExecutorService => Future[A]

  extension [A](pa: Par[A]) def run(s: ExecutorService): Future[A] = pa(s)

  def unit[A](a: A): Par[A] =
    es => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  extension [A](pa: Par[A]) def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    es => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  extension [A](pa: Par[A]) def map2Timeouts[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C]:
      private val futureA = pa(es)
      private val futureB = pb(es)
      @volatile private var cache: Option[C] = None

      def isDone = cache.isDefined
      def get() = get(Long.MaxValue, TimeUnit.NANOSECONDS)

      def get(timeout: Long, units: TimeUnit) =
        val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, units)
        val started = System.nanoTime
        val a = futureA.get(timeoutNanos, TimeUnit.NANOSECONDS)
        val elapsed = System.nanoTime - started
        val b = futureB.get(timeoutNanos - elapsed, TimeUnit.NANOSECONDS)
        val c = f(a, b)
        cache = Some(c)
        c

      def isCancelled = futureA.isCancelled || futureB.isCancelled
      def cancel(evenIfRunning: Boolean) =
        futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { def call = a(es).get })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  extension [A](pa: Par[A]) def map[B](f: A => B): Par[B] =
    pa.map2(unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) =
    parList.map(_.sorted)

  def sequenceSimple[A](pas: List[Par[A]]): Par[List[A]] =
    pas.foldRight(unit(List.empty[A]))((pa, acc) => pa.map2(acc)(_ :: _))

  // This implementation forks the recursive step off to a new logical thread,
  // making it effectively tail-recursive. However, we are constructing
  // a right-nested parallel program, and we can get better performance by
  // dividing the list in half, and running both halves in parallel.
  // See `sequenceBalanced` below.
  def sequenceRight[A](pas: List[Par[A]]): Par[List[A]] =
    pas match
      case Nil => unit(Nil)
      case h :: t => h.map2(fork(sequenceRight(t)))(_ :: _)

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if pas.isEmpty then unit(IndexedSeq.empty)
    else if pas.size == 1 then pas.head.map(a => IndexedSeq(a))
    else
      val (l, r) = pas.splitAt(pas.size / 2)
      sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)

  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    sequenceBalanced(pas.toIndexedSeq).map(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val pars: List[Par[List[A]]] =
      l.map(asyncF(a => if f(a) then List(a) else List()))
    sequence(pars).map(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if cond.run(es).get then t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val ind = n.run(es).get // Full source files
      choices(ind).run(es)
    }

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(a.map(b => if b then 0 else 1))(List(ifTrue, ifFalse))

  def choiceMap[K, V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es => {
      val k = key.run(es).get
      choices(k).run(es)
    }

  extension [A](pa: Par[A]) def chooser[B](choices: A => Par[B]): Par[B] =
    es => {
      val k = pa.run(es).get
      choices(k).run(es)
    }

  /* `chooser` is usually called `flatMap` or `bind`. */
  extension [A](pa: Par[A]) def flatMap[B](choices: A => Par[B]): Par[B] =
    es => {
      val k = pa.run(es).get
      choices(k).run(es)
    }

  def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
    flatMap(p)(b => if b then t else f)

  def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(p)(i => choices(i))

  // see nonblocking implementation in `Nonblocking.scala`
  def join[A](a: Par[Par[A]]): Par[A] =
    es => a.run(es).get().run(es)

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  extension [A](pa: Par[A]) def flatMapViaJoin[B](f: A => Par[B]): Par[B] =
    join(pa.map(f))

object Examples:
  import Par.*
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if ints.size <= 1 then
      ints.headOption.getOrElse(0) // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else
      val (l, r) = ints.splitAt(ints.size / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.

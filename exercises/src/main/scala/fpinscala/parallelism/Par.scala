package fpinscala.parallelism

import java.util.concurrent._

import scala.language.implicitConversions
import java.util.concurrent.Executors

import fpinscala.state.RNG

import scala.concurrent.duration._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    def isDone = a.isDone && b.isDone
    def get(timeout: Long, units: TimeUnit) = {
      val timeoutMillis = TimeUnit.MILLISECONDS.convert(timeout, units)
      val aValue = a.get(timeout, units)
      val before = System.currentTimeMillis()
      val timeForA = System.currentTimeMillis() - before
      val bValue = b.get(timeoutMillis - timeForA, TimeUnit.MILLISECONDS)
      f(aValue, bValue)
    }
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    override def get(): C = f(a.get(), b.get())
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(), bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map2Fixed[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => Map2Future(a(es), b(es), f)

  def map22[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    flatMap(a)(a => flatMap(b)(b => unit(f(a, b))))

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit({
      //      println("submit")
      new Callable[A] {
        def call = a(es).get
      }
    })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(c => if (c) 1 else 0))(f :: t :: Nil)

  def choice3[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceF(cond)(if (_) t else f)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => choices(run(es)(n).get)(es)

  def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    choiceMap[Int, A](n)(choices.zipWithIndex.map({ case (value, i) => i -> value }).toMap)

  def choiceN3[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    choiceF(n)(choices(_))

  def choiceMap[K, V](k: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => choices(run(es)(k).get)(es)

  def choiceMap2[K, V](k: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    choiceF(k)(choices(_))

  def choiceF[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    es => f(run(es)(a).get)(es)

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(a).get()(es)

  def join2[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map[A, Par[B]](a)(f))

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  def lazyUnit[A](a: A) = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (ps.isEmpty) unit(Nil.toIndexedSeq)
    else if (ps.size == 1) map(ps.head)(Vector(_))
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequenceSimple[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(Nil))((acc, p) => map2(acc, p)(_ :: _))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fas = as.map(asyncF[A, Option[A]](a => Some(a).filter(f)))
    map(sequence(fas))(_.flatten)
  }

  def parReduce[A](as: IndexedSeq[A], z: A)(f: (A, A) => A): Par[A] = fork {
    if (as.isEmpty) unit(z)
    else if (as.size == 1) unit(as.head)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(parReduce(l, z)(f), parReduce(r, z)(f))(f)
    }
  }

  def parSum(ints: IndexedSeq[Int]): Par[Int] = parReduce(ints, 0)(longAdd)

  def parCountLetters(paragraphs: IndexedSeq[String]): Par[Int] = parReduce(paragraphs.map(_.length), 0)(longAdd)

  def longAdd(x: Int, y: Int) = {
    Thread.sleep(100)
    println("add")
    x + y
  }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
    map2Fixed(map2Fixed(a, b)((_, _)), c)({ case ((a, b), c) => f(a, b, c) })

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
    map2Fixed(map2Fixed(map2Fixed(a, b)((_, _)), c)((_, _)), d)({ case (((a, b), c), d) => f(a, b, c, d) })

  def map5[A, B, C, D, E, G](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => G): Par[G] =
    map2Fixed(map2Fixed(map2Fixed(map2Fixed(a, b)((_, _)), c)((_, _)), d)((_, _)), e)({ case ((((a, b), c), d), e) => f(a, b, c, d, e) })

  class ParOps[A](p: Par[A]) {
    def eq2(p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)
  }
}

object Examples {

  def sum(ints: IndexedSeq[Int])(f: (Int, Int) => Int): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      f(sum(l)(f), sum(r)(f)) // Recursively sum both halves and add the results together.
    }
}

object App extends App {
  //  val f = Par.run(Executors.newFixedThreadPool(8))(Par.parSum((1 :: 2 :: 3 :: Nil).toIndexedSeq))
  //  println(f.get)

  val before = System.currentTimeMillis()

  //  val f0 = Examples.sum(Seq.fill(1000)(2).toIndexedSeq)(Par.longAdd)
  //  println(f0)

  //  val f2 = Par.run(Executors.newCachedThreadPool)(Par.parCountLetters(Seq.fill(1000)("sjlkdfjsdlkfjsdf").toIndexedSeq))
  val f = Par.run(Executors.newCachedThreadPool)(Par.parSum(Seq.fill(1000)(2).toIndexedSeq))
  println(f.get)

  println(s"time - ${System.currentTimeMillis() - before}")
}
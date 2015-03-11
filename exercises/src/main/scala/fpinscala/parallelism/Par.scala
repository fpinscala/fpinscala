package fpinscala.parallelism

import java.util.concurrent._

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

  private case class Map2Future[A, B, C](fa: Future[A], fb: Future[B], f: (A, B) => C)
    extends Future[C] {

    def isDone = fa.isDone && fb.isDone

    def get: C = f(fa.get, fb.get)

    def get(timeout: Long, units: TimeUnit): C = {
      val (a, elapsed) = time(fa.get(timeout, units))
      val left = Duration(timeout, units) - elapsed
      val b = fb.get(left.toNanos, TimeUnit.NANOSECONDS)
      f(a, b)
    }

    private def time[A](block: => A): (A, Duration) = {
      val before = System.currentTimeMillis
      val value = block
      val after = System.currentTimeMillis
      (value, (after - before).millis)
    }

    def isCancelled = fa.isCancelled || fb.isCancelled

    def cancel(evenIfRunning: Boolean): Boolean =
      fa.cancel(evenIfRunning) &
      fb.cancel(evenIfRunning)
  }
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(pa, pb)((_, _)), pc) { case ((a, b), c) =>
      f(a, b, c)
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]) = parList.map(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List.empty[A])) { (a, tail) =>
      map2(a, tail)(_ :: _)
    }
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(ps.map(asyncF(f)))
  }

  def parFilter_mapReduce[A](as: IndexedSeq[A])(p: A => Boolean): Par[List[A]] = {
    mapReduce(as)(List.empty[A])(Option(_).filter(p).toList)(_ ::: _)
  }

  def parFilter[A](as: List[A])(p: A => Boolean): Par[List[A]] =
    if (as.length <= 1)
      unit(as.filter(p))
    else {
      val (left, right) = as.splitAt(as.length / 2)
      map2(fork(parFilter(left)(p)), fork(parFilter(right)(p)))(_ ::: _)
    }

  def mapReduce0[A, B, C](as: List[A])(z: B)(map: A => B)(reduce: (B, B) => B): Par[B] = {
    as match {
      case Nil => unit(z)
      case a :: tail => map2(lazyUnit(map(a)), mapReduce0(tail)(z)(map)(reduce))(reduce)
    }
  }

  def mapReduce[A, B, C](as: IndexedSeq[A])(z: B)(map: A => B)(reduce: (B, B) => B): Par[B] = {
    if (as.length <= 1)
      unit(as.headOption.fold(z)(map))
    else {
      val (left, right) = as.splitAt(as.length / 2)
      map2(
        fork(mapReduce(left)(z)(map)(reduce)),
        fork(mapReduce(right)(z)(map)(reduce))
      )(reduce)
    }
  }

  def reduce[A](as: IndexedSeq[A])(f: (A, A) => A): Par[Option[A]] = {
    mapReduce(as)(Option.empty[A])(Some.apply) {
      case (Some(left), Some(right)) => Some(f(left, right))
      case (Some(left), None) => Some(left)
      case (None, Some(right)) => Some(right)
      case (None, None) => None
    }
  }

  def wordCountForParagraphs(paragraphs: IndexedSeq[String]): Par[Int] =
    mapReduce(paragraphs)(0)(wordCount)(_ + _)

  def wordCount(paragraph: String): Int =
    paragraph.split(raw"[^\w]+").filter(_.trim.nonEmpty).length

//  /* Gives us infix syntax for `Par`. */
//  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  implicit class ParOps[A](val p: Par[A]) extends AnyVal {
    def map[B](f: A => B): Par[B] = map2(p, unit(()))((a, _) => f(a))
  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Par[Int] = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0) // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      map2(fork(sum(l)), fork(sum(r)))(_ + _) // Recursively sum both halves and add the results together.
    }

  def sum_mapReduce(ints: IndexedSeq[Int]): Par[Int] =
    mapReduce(ints)(0)(identity)(_ + _)

  def sum_reduce(ints: IndexedSeq[Int]): Par[Int] =
    reduce(ints)(_ + _).map(_.getOrElse(0))

  def max(ints: IndexedSeq[Int]): Par[Option[Int]] =
    reduce(ints)(_ max _)

}

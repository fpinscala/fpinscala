package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /* 
  The implementation of `unit` does not use the `ExecutorService`, it simply returns a `Future` directly. 
  */
  def unit[A](a: A): Par[A] = 
    (es: ExecutorService) => UnitFuture(a)
  
  /* Simple future for wrapping a constant value. */
  case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  
  /* 
  Notice this implementation does not evaluate the call to `f` in a separate logical thread. This is in keeping with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
  
  This implementation does _not_ respect timeouts. In order to respect timeouts, we need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.  
  */
  def map2_simple[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  
  /* This version respects timeouts. See `Map2Future` below. */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }
  
  /* 
  This is the simplest, most natural implementation, but there are some problems with it--for one, the outer `Callable` will block waiting for the 'inner' task to complete. Since this blocked thread occupies a thread in our thread pool or whatever resource backs the `ExecutorService`, this implies we're losing out on some potential parallelism (essentially, we are using two threads when one should do). This is a symptom of a more serious problem with the implementation that we'll discuss later in the chapter. 
  */
  def fork_simple[A](a: => Par[A]): Par[A] = 
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })
  
  
  /* 
  Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel. We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation of pure values won't affect results).
  */
  case class Map2Future[A,B,C](a: Future[A], b: Future[B], 
                               f: (A,B) => C) extends Future[C] {
    var cache: Option[C] = None
    def isDone = cache.isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean) = 
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C = 
      compute(TimeUnit.MILLISECONDS.convert(timeout, units))
  
    private def compute(timeoutMs: Long): C = cache match {
      case Some(c) => c 
      case None =>  
        val start = System.currentTimeMillis
        val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
        val stop = System.currentTimeMillis; val at = stop-start
        val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS) 
        cache = Some(f(ar, br))
        cache.get
    }
  }

  def asyncF[A,B](f: A => B): A => Par[B] = 
    a => fork(unit(f(a)))

  def map[A,B](fa: Par[A])(f: A => B): Par[B] = 
    map2(fa, unit(()))((a,_) => f(a))

  def sortPar(l: Par[List[Int]]) = map(l)(_.sorted)

  def sequence[A](l: List[Par[A]]): Par[List[A]] = 
    l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = 
      l map (asyncF((a: A) => if (f(a)) List(a) else List())) 
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  /* 
  The correctness of this implementation requires only that the `ExecutorService` begins executing tasks in the order they are submitted. This enables us to safely call `innerF.get`. (You may want to try proving to yourself that this cannot deadlock)
  */
  def fork[A](p: => Par[A]): Par[A] = {
    es => {
      val latch = new CountDownLatch(1)
      var result: Option[A] = None
      var innerF: Future[A] = null
      var resultF: Future[_] = null
      val f = es.submit(new Runnable { 
        def run = { 
          innerF = p(es)
          resultF = es.submit(new Runnable {
            def run = { result = Some(innerF.get); latch.countDown }    
          })
        }
      })
      new Future[A] {
        def get = { latch.await; result.get }
        def get(timeout: Long, units: TimeUnit) = {
          latch.await(timeout, units)
          result.get
        }
        def isDone = latch.getCount == 0
        def cancel(b: Boolean) = {
          isCancelled = 
            isCancelled ||
            f.cancel(b) || 
            (innerF != null && innerF.cancel(b)) || 
            (resultF != null && resultF.cancel(b))
          isCancelled
        }
        var isCancelled = false 
      }
    }
  }

  /* This implementation uses `get` directly and does not propagate timeouts. */
  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    es => if (a(es).get) ifTrue(es) else ifFalse(es)

  def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = 
    es => choices(a(es).get)(es)
  
  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(b => if (b) 1 else 0))(List(ifTrue, ifFalse))

  def chooser[A,B](a: Par[A])(choices: A => Par[B]): Par[B] = 
    es => choices(a(es).get)(es)
  
  /* `chooser` is usually called `flatMap` or `bind`. */
  def flatMap[A,B](a: Par[A])(choices: A => Par[B]): Par[B] = 
    es => choices(a(es).get)(es)
  
  def choiceViaFlatMap[A](p: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    flatMap(p)(b => if (b) ifTrue else ifFalse)
  
  def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(p)(i => choices(i))

  /* 
  This implementation is not safe for execution on bounded thread pools, and it also does not preserve timeouts. Can you see why? You may wish to try implementing a nonblocking version like was done for `fork`.  
  */
  def join[A](a: Par[Par[A]]): Par[A] = 
    es => a(es).get.apply(es)
  
  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = 
    flatMap(a)(a => a)
  
  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] = 
    join(map(p)(f))
  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {

  }
}

object Examples {
  def sum(as: IndexedSeq[Int]): Int =
    if (as.size <= 1) as.headOption getOrElse 0 // Hints and standalone answers
    else { 
      val (l,r) = as.splitAt(as.length/2) 
      sum(l) + sum(r)
    }
}
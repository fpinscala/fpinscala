package fpinscala.answers.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object Nonblocking:

  opaque type Future[+A] = (A => Unit) => Unit

  opaque type Par[+A] = ExecutorService => Future[A]

  object Par:

    extension [A](p: Par[A]) def run(es: ExecutorService): A =
      val ref = new AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
      latch.await // Block until the `latch.countDown` is invoked asynchronously
      ref.get // Once we've passed the latch, we know `ref` has been set, and return its value

    def unit[A](a: A): Par[A] =
      es => cb => cb(a)

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] =
      es => cb => cb(a)

    def fork[A](a: => Par[A]): Par[A] =
      es => cb => eval(es)(a(es)(cb))

    /**
     * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
     * This will come in handy in Chapter 13.
     */
    def async[A](f: (A => Unit) => Unit): Par[A] = 
      es => cb => f(cb)

    /**
     * Helper function, for evaluating an action
     * asynchronously, using the given `ExecutorService`.
     */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    extension [A](p: Par[A]) def map2[B, C](p2: Par[B])(f: (A, B) => C): Par[C] =
      es => cb => {
        var ar: Option[A] = None
        var br: Option[B] = None
        // this implementation is a little too liberal in forking of threads -
        // it forks a new logical thread for the actor and for stack-safety,
        // forks evaluation of the callback `cb`
        val combiner = Actor[Either[A,B]](es) {
          case Left(a) =>
            if br.isDefined then Par.eval(es)(cb(f(a, br.get)))
            else ar = Some(a)
          case Right(b) =>
            if ar.isDefined then Par.eval(es)(cb(f(ar.get, b)))
            else br = Some(b)
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }

    extension [A](p: Par[A]) def map[B](f: A => B): Par[B] =
        es => cb => p(es)(a => eval(es)(cb(f(a))))

    extension [A](p: Par[A]) def flatMap[B](f: A => Par[B]): Par[B] =
        es => cb => p(es)(a => f(a)(es)(cb))

    extension [A](p: Par[A]) def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match
        case Nil => unit(Nil)
        case h :: t => h.map2(fork(sequence(t)))(_ :: _)

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if as.isEmpty then unit(Vector())
      else if as.length == 1 then map(as.head)(a => Vector(a))
      else
        val (l, r) = as.splitAt(as.length / 2)
        sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))

    // exercise answers

    /*
     * We can implement `choice` as a new primitive.
     *
     * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
     * some `Par`, `p`, is the idiom for running `p`, and registering
     * a callback to be invoked when its result is available. The
     * result will be bound to `result` in the function passed to
     * `p(es)`.
     *
     * If you find this code difficult to follow, you may want to
     * write down the type of each subexpression and follow the types
     * through the implementation. What is the type of `p(es)`? What
     * about `t(es)`? What about `t(es)(cb)`?
     */
    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => cb => cond(es) { b =>
        if b then eval(es)(t(es)(cb))
        else eval(es)(f(es)(cb))
      }

    /* The code here is very similar. */
    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      es => cb => p(es)(ind => eval(es)(ps(ind)(es)(cb)))

    def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(cond.map(b => if b then 0 else 1))(List(t, f))

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => cb => key(es)(k => choices(k)(es)(cb))

    /* `chooser` is usually called `flatMap` or `bind`. */
    def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      p.flatMap(f)

    def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      p.flatMap(b => if b then t else f)

    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      p.flatMap(i => choices(i))

    def join[A](ppa: Par[Par[A]]): Par[A] =
      es => cb => ppa(es)(pa => eval(es)(pa(es)(cb)))

    def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] =
      ppa.flatMap(identity)

    def flatMapViaJoin[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
      join(pa.map(f))

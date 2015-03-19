package fpinscala.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object Nonblocking {

  trait Future[+A] {
    private[parallelism] def apply(f: Try[A] => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new java.util.concurrent.atomic.AtomicReference[Try[A]] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
      latch.await // Block until the `latch.countDown` is invoked asynchronously
      ref.get.get // Once we've passed the latch, we know `ref` has been set, and return its value
    }

    def unit[A](a: A): Par[A] = fromTry(Success(a))
    def failure[A](ex: Throwable): Par[A] = fromTry(Failure(ex))

    def fromTry[A](a: Try[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: Try[A] => Unit): Unit =
          cb(a)
      }

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: Try[A] => Unit): Unit =
          cb(Try(a))
      }

    def forkValue[A](a: => A): Par[A] = fork(lazyUnit(a))

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: Try[A] => Unit): Unit = {
          val sa =
            try a
            catch {
              case NonFatal(e) => failure(e)
            }

          eval(es)(sa(es)(cb))
        }
      }

    /**
     * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
     * This will come in handy in Chapter 13.
     */
    def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
      def apply(k: Try[A] => Unit) = ??? ///f(k)
    }

    /**
     * Helper function, for evaluating an action
     * asynchronously, using the given `ExecutorService`.
     */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })


    def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {

        def apply(cb: Try[C] => Unit): Unit = {

          def call(tryA: Try[A], tryB: Try[B]): Unit = {
            val tryC = for {
              a <- tryA
              b <- tryB
            } yield {
              f(a, b)
            }
            eval(es)(cb(tryC))
          }

          var ar: Option[Try[A]] = None
          var br: Option[Try[B]] = None
          val combiner = Actor[Either[Try[A], Try[B]]](es) {
            case Left(a) =>
              if (br.isDefined) call(a, br.get)
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) call(ar.get, b)
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    // specialized version of `map`
    def map[A,B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: Try[B] => Unit): Unit =
          p(es)(a => eval(es) { cb(a map f) })
      }

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

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
    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: Try[A] => Unit): Unit =
          p(es) { bTry =>
            bTry.map { b =>
              if (b) eval(es) { t(es)(cb) }
              else eval(es) { f(es)(cb) }
            }
          }
      }

    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: Try[A] => Unit): Unit =
          p(es) { bTry =>
            bTry.map { index =>
              eval(es) { ps(index)(es)(cb) }
            }
          }
      }

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(a.map(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    def choiceMap[K,V](p: Par[K])(ps: Map[K,Par[V]]): Par[V] =
      p.flatMap(ps(_))

    // see `Nonblocking.scala` answers file. This function is usually called something else!
    def chooser[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      p flatMap f

    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      es => new Future[B] {
        def apply(cb: Try[B] => Unit): Unit =
          p(es) {
            _.map { a =>
              eval(es) { f(a)(es)(cb) }
            }
          }
      }

    def choiceViaChooser[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      p.flatMap { if (_) t else f }

    def choiceNChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      p.flatMap(choices(_))

    def join[A](p: Par[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: Try[A] => Unit): Unit =
          p(es) {
            _.map { a =>
              eval(es) { a(es)(cb) }
            }
          }
      }

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      a.flatten

    def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      p.map(f).flatten

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      sequence(ps.map(asyncF(f)))
    }

    /* Gives us infix syntax for `Par`. */
    //implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2`
    implicit class ParOps[A](val p: Par[A]) extends AnyVal {
      def map[B](f: A => B): Par[B] = p.flatMap { a => unit(f(a)) }

      def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)

      def map2ViaFlatMap[B,C](pb: Par[B])(f: (A, B) => C): Par[C] =
        for {
          a <- p
          b <- pb
        } yield f(a, b)


      def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))

      def flatMap[B](f: A => Par[B]): Par[B] =
        Par.flatMap(p)(f)

      def willEqual[B](that: Par[B]): Par[Boolean] =
        map2(that)(_ == _)
    }

    implicit class ParListParOps[A](val p: Par[Par[A]]) extends AnyVal {
      def flatten: Par[A] = p.flatMap(identity)
    }
  }
}

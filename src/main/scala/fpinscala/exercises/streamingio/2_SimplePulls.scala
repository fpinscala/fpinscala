package fpinscala.exercises.streamingio

import fpinscala.answers.iomonad.{IO, Monad}
import fpinscala.answers.monoids.Monoid

object SimplePulls:

  enum Pull[+O, +R]:
    case Result[+R](result: R) extends Pull[Nothing, R]
    case Output[+O](value: O) extends Pull[O, Unit]
    case FlatMap[X, +O, +R](source: Pull[O, X], f: X => Pull[O, R]) extends Pull[O, R]

    def step: Either[R, (O, Pull[O, R])] = this match
      case Result(r) => Left(r)
      case Output(o) => Right(o, Pull.done)
      case FlatMap(source, f) => 
        source match
          case FlatMap(s2, g) => s2.flatMap(x => g(x).flatMap(y => f(y))).step
          case other => other.step match
            case Left(r) => f(r).step
            case Right((hd, tl)) => Right((hd, tl.flatMap(f)))

    @annotation.tailrec
    final def fold[A](init: A)(f: (A, O) => A): (R, A) = 
      step match
        case Left(r) => (r, init)
        case Right((hd, tl)) => tl.fold(f(init, hd))(f)

    def toList: List[O] =
      fold(List.newBuilder[O])((bldr, o) => bldr += o)(1).result

    def flatMap[O2 >: O, R2](f: R => Pull[O2, R2]): Pull[O2, R2] =
      FlatMap(this, f)

    def >>[O2 >: O, R2](next: => Pull[O2, R2]): Pull[O2, R2] =
      flatMap(_ => next)

    def map[R2](f: R => R2): Pull[O, R2] =
      flatMap(r => Result(f(r)))

    def repeat: Pull[O, Nothing] =
      this >> repeat

    def uncons: Pull[Nothing, Either[R, (O, Pull[O, R])]] =
      Pull.done >> Result(step)

    def take(n: Int): Pull[O, Option[R]] =
      if n <= 0 then Result(None)
      else uncons.flatMap {
        case Left(r) => Result(Some(r))
        case Right((hd, tl)) => Output(hd) >> tl.take(n - 1)
      }

    // Exercise 15.3
    def drop(n: Int): Pull[O, R] =
      ???

    // Exercise 15.3
    def takeWhile(f: O => Boolean): Pull[O, Pull[O, R]] =
      ???
    
    // Exercise 15.3
    def dropWhile(f: O => Boolean): Pull[Nothing, Pull[O, R]] =
      ???

    def mapOutput[O2](f: O => O2): Pull[O2, R] =
      uncons.flatMap {
        case Left(r) => Result(r)
        case Right((hd, tl)) => Output(f(hd)) >> tl.mapOutput(f)
      }

    def filter(p: O => Boolean): Pull[O, R] =
      uncons.flatMap {
        case Left(r) => Result(r)
        case Right((hd, tl)) =>
          (if p(hd) then Output(hd) else Pull.done) >> tl.filter(p)
      }

    def count: Pull[Int, R] =
      def go(total: Int, p: Pull[O, R]): Pull[Int, R] =
        p.uncons.flatMap {
          case Left(r) => Result(r)
          case Right((_, tl)) =>
            val newTotal = total + 1
            Output(newTotal) >> go(newTotal, tl)
        }
      Output(0) >> go(0, this)

    // Exercise 15.4
    def tally[O2 >: O](using m: Monoid[O2]): Pull[O2, R] =
      ???

    def mapAccumulate[S, O2](init: S)(f: (S, O) => (S, O2)): Pull[O2, (S, R)] =
      uncons.flatMap {
        case Left(r) => Result((init, r))
        case Right((hd, tl)) =>
          val (s, out) = f(init, hd)
          Output(out) >> tl.mapAccumulate(s)(f)
      }

    // Exercise 15.6
    def countViaMapAccumulate: Pull[Int, R] =
      ???

    // Exercise 15.6
    def tallyViaMapAccumulate[O2 >: O](using m: Monoid[O2]): Pull[O2, R] =
      ???

  object Pull:
    val done: Pull[Nothing, Unit] = Result(())

    def fromList[O](os: List[O]): Pull[O, Unit] =
      os match
        case Nil => done
        case hd :: tl => Output(hd) >> fromList(tl)

    def fromLazyList[O](os: LazyList[O]): Pull[O, Unit] =
      os match
        case LazyList() => done
        case hd #:: tl => Output(hd) >> fromLazyList(tl)

    def unfold[O, R](init: R)(f: R => Either[R, (O, R)]): Pull[O, R] =
      f(init) match
        case Left(r) => Result(r)
        case Right((o, r2)) => Output(o) >> unfold(r2)(f)

    // Exercise 15.1
    def fromListViaUnfold[O](os: List[O]): Pull[O, Unit] =
      ???

    // Exercise 15.1
    def fromLazyListViaUnfold[O](os: LazyList[O]): Pull[O, Unit] =
      ???

    def continually[O](o: O): Pull[O, Nothing] =
      Output(o) >> continually(o)

    def continuallyViaRepeat[O](o: O): Pull[O, Nothing] =
      Output(o).repeat

    // Exercise 15.2
    def iterate[O](initial: O)(f: O => O): Pull[O, Nothing] =
      ???

    extension [R](self: Pull[Int, R])
      // Exercise 15.5
      def slidingMean(n: Int): Pull[Double, R] =
        ???

      // Exercise 15.6
      def slidingMeanViaMapAccumulate(n: Int): Pull[Double, R] =
        ???

    given [O]: Monad[[x] =>> Pull[O, x]] with
      def unit[A](a: => A): Pull[O, A] = Result(a)
      extension [A](pa: Pull[O, A])
        def flatMap[B](f: A => Pull[O, B]): Pull[O, B] =
          pa.flatMap(f)

    extension [O](self: Pull[O, Unit])
      def flatMapOutput[O2](f: O => Pull[O2, Unit]): Pull[O2, Unit] =
        self.uncons.flatMap {
          case Left(()) => Result(())
          case Right((hd, tl)) =>
            f(hd) >> tl.flatMapOutput(f)
        }
    val outputMonad: Monad[[x] =>> Pull[x, Unit]] = new:
      def unit[A](a: => A): Pull[A, Unit] = Output(a)
      extension [A](pa: Pull[A, Unit])
        def flatMap[B](f: A => Pull[B, Unit]): Pull[B, Unit] =
          pa.flatMapOutput(f)

    extension [O](self: Pull[O, Unit])
      def toStream: Stream[O] = self
  
  opaque type Stream[+O] = Pull[O, Unit]
  object Stream:
    def apply[O](os: O*): Stream[O] =
      Pull.fromList(os.toList).toStream
    extension [O](self: Stream[O])
      def toPull: Pull[O, Unit] = self

      def fold[A](init: A)(f: (A, O) => A): A = 
        self.fold(init)(f)(1)

      def toList: List[O] =
        self.toList

      def take(n: Int): Stream[O] =
        self.take(n).void

      def filter(p: O => Boolean): Stream[O] =
        self.filter(p).toStream

      def ++(that: => Stream[O]): Stream[O] =
        self >> that

    given Monad[Stream] with
      def unit[A](a: => A): Stream[A] = Pull.Output(a)
      extension [A](sa: Stream[A])
        def flatMap[B](f: A => Stream[B]): Stream[B] =
          sa.flatMapOutput(f)

  type Pipe[-I, +O] = Stream[I] => Stream[O]

end SimplePulls

object SimplePullExamples:
  import SimplePulls.{Pipe, Stream, Pull}

  val nonEmpty: Pipe[String, String] =
    _.filter(_.nonEmpty)

  val lowerCase: Pipe[String, String] =
    _.map(_.toLowerCase)

  val normalize: Pipe[String, String] =
    nonEmpty andThen lowerCase

  val lines = Stream("Hello", "", "World!")
  val normalized = normalize(lines)
  import scala.util.chaining.scalaUtilChainingOps
  val normalized2 = lines.pipe(normalize)
  val normalized3 = lines.pipe(nonEmpty).pipe(lowerCase)

  def count[A]: Pipe[A, Int] = _.toPull.count.void.toStream

  // Exercise 15.7
  def exists[I](f: I => Boolean): Pipe[I, Boolean] =
    ???

  // Exercise 15.7
  def existsHalting[I](f: I => Boolean): Pipe[I, Boolean] =
    ???

  def countGt40K[I]: Pipe[I, Boolean] =
    count andThen existsHalting(_ > 40000)

  def fromIterator[O](itr: Iterator[O]): Stream[O] =
    Pull.unfold(itr)(itr =>
      if itr.hasNext then Right((itr.next(), itr))
      else Left(itr)
    ).void.toStream

  def processFile[A](
    file: java.io.File,
    p: Pipe[String, A],
  )(using m: Monoid[A]): IO[A] = IO {
    val source = scala.io.Source.fromFile(file)
    try fromIterator(source.getLines).pipe(p).fold(m.empty)(m.combine)
    finally source.close()
  }

  def checkFileForGt40K(file: java.io.File): IO[Boolean] =
    processFile(file, count andThen exists(_ > 40000))(using Monoid.booleanOr)

  def toCelsius(fahrenheit: Double): Double =
    (5.0 / 9.0) * (fahrenheit - 32.0)

  def convert(inputFile: String, outputFile: String): IO[Unit] =
    ???
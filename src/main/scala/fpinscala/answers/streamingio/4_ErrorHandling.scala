package fpinscala.answers.streamingio

import fpinscala.answers.iomonad.{IO, Monad}
import fpinscala.answers.monoids.Monoid
import scala.util.{Success, Failure}

object ErrorHandling:

  type Nothing1[A] = Nothing

  enum Pull[+F[_], +O, +R]:
    case Result[+R](result: R) extends Pull[Nothing, Nothing, R]
    case Output[+O](value: O) extends Pull[Nothing, O, Unit]
    case Eval[+F[_], R](action: F[R]) extends Pull[F, Nothing, R]
    case FlatMap[+F[_], X, +O, +R](
      source: Pull[F, O, X], f: X => Pull[F, O, R]) extends Pull[F, O, R]
    case Uncons[+F[_], +O, +R](source: Pull[F, O, R])
      extends Pull[F, Nothing, Either[R, (O, Pull[F, O, R])]]
    case Handle[+F[_], +O, +R](
      source: Pull[F, O, R], handler: Throwable => Pull[F, O, R])
      extends Pull[F, O, R]
    case Error(t: Throwable) extends Pull[Nothing, Nothing, Nothing]

    def step[F2[x] >: F[x], O2 >: O, R2 >: R](
      using F: MonadThrow[F2]
    ): F2[Either[R2, (O2, Pull[F2, O2, R2])]] =
      this match
        case Result(r) => F.unit(Left(r))
        case Output(o) => F.unit(Right(o, Pull.done))
        case Eval(action) => action.map(Left(_))
        case Uncons(source) =>
          source.step.map(s => Left(s.asInstanceOf[R2]))
        case Handle(source, f) =>
          source match
            case Handle(s2, g) =>
              s2.handleErrorWith(x => g(x).handleErrorWith(y => f(y))).step
            case other =>
              other.step.map {
                case Right((hd, tl)) => Right((hd, Handle(tl, f)))
                case Left(r) => Left(r)
              }.handleErrorWith(t => f(t).step)
        case Error(t) => F.raiseError(t)
        case FlatMap(source, f) => 
          source match
            case FlatMap(s2, g) =>
              s2.flatMap(x => g(x).flatMap(y => f(y))).step
            case other => other.step.flatMap {
              case Left(r) => f(r).step
              case Right((hd, tl)) => F.unit(Right((hd, tl.flatMap(f))))
            }

    def fold[F2[x] >: F[x], R2 >: R, A](init: A)(f: (A, O) => A)(
      using F: MonadThrow[F2]
    ): F2[(R2, A)] = 
      step.flatMap {
        case Left(r) => F.unit((r, init))
        case Right((hd, tl)) => tl.fold(f(init, hd))(f)
      }

    def toList[F2[x] >: F[x]: MonadThrow, O2 >: O]: F2[List[O2]] =
      fold(List.newBuilder[O])((bldr, o) => bldr += o).map(_(1).result)

    def flatMap[F2[x] >: F[x], O2 >: O, R2](f: R => Pull[F2, O2, R2]): Pull[F2, O2, R2] =
      FlatMap(this, f)

    def >>[F2[x] >: F[x], O2 >: O, R2](next: => Pull[F2, O2, R2]): Pull[F2, O2, R2] =
      flatMap(_ => next)

    def map[R2](f: R => R2): Pull[F, O, R2] =
      this.flatMap(r => Result(f(r)))

    def repeat: Pull[F, O, Nothing] =
      this >> repeat

    def uncons: Pull[F, Nothing, Either[R, (O, Pull[F, O, R])]] =
      Uncons(this)

    def take(n: Int): Pull[F, O, Option[R]] =
      if n <= 0 then Result(None)
      else uncons.flatMap {
        case Left(r) => Result(Some(r))
        case Right((hd, tl)) => Output(hd) >> tl.take(n - 1)
      }

    def takeWhile(f: O => Boolean): Pull[F, O, Pull[F, O, R]] =
      uncons.flatMap {
        case Left(r) => Result(Result(r))
        case Right((hd, tl)) =>
          if f(hd) then Output(hd) >> tl.takeWhile(f)
          else Result(Output(hd) >> tl)
      }

    def dropWhile(f: O => Boolean): Pull[F, Nothing, Pull[F, O, R]] =
      uncons.flatMap {
        case Left(r) => Result(Result(r))
        case Right((hd, tl)) =>
          if f(hd) then tl.dropWhile(f)
          else Result(Output(hd) >> tl)
      }

    def mapOutput[O2](f: O => O2): Pull[F, O2, R] =
      uncons.flatMap {
        case Left(r) => Result(r)
        case Right((hd, tl)) => Output(f(hd)) >> tl.mapOutput(f)
      }

    def filter(p: O => Boolean): Pull[F, O, R] =
      uncons.flatMap {
        case Left(r) => Result(r)
        case Right((hd, tl)) =>
          (if p(hd) then Output(hd) else Pull.done) >> tl.filter(p)
      }

    def count: Pull[F, Int, R] =
      def go(total: Int, p: Pull[F, O, R]): Pull[F, Int, R] =
        p.uncons.flatMap {
          case Left(r) => Result(r)
          case Right((_, tl)) =>
            val newTotal = total + 1
            Output(newTotal) >> go(newTotal, tl)
        }
      Output(0) >> go(0, this)

    def tally[O2 >: O](using m: Monoid[O2]): Pull[F, O2, R] =
      def go(total: O2, p: Pull[F, O, R]): Pull[F, O2, R] =
        p.uncons.flatMap {
          case Left(r) => Result(r)
          case Right((hd, tl)) =>
            val newTotal = m.combine(total, hd)
            Output(newTotal) >> go(newTotal, tl)
        }
      Output(m.empty) >> go(m.empty, this)

    def mapAccumulate[S, O2](init: S)(f: (S, O) => (S, O2)): Pull[F, O2, (S, R)] =
      uncons.flatMap {
        case Left(r) => Result((init, r))
        case Right((hd, tl)) =>
          val (s, out) = f(init, hd)
          Output(out) >> tl.mapAccumulate(s)(f)
      }

    def handleErrorWith[F2[x] >: F[x], O2 >: O, R2 >: R](handler: Throwable => Pull[F2, O2, R2]): Pull[F2, O2, R2] =
      Pull.Handle(this, handler)

  object Pull:

    val done: Pull[Nothing, Nothing, Unit] = Result(())

    def unfold[O, R](init: R)(f: R => Either[R, (O, R)]): Pull[Nothing1, O, R] =
      f(init) match
        case Left(r) => Result(r)
        case Right((o, r2)) => Output(o) >> unfold(r2)(f)

    def unfoldEval[F[_], O, R](init: R)(f: R => F[Either[R, (O, R)]]): Pull[F, O, R] =
      Pull.Eval(f(init)).flatMap {
        case Left(r) => Result(r)
        case Right((o, r2)) => Output(o) >> unfoldEval(r2)(f)
      }

    extension [F[_], R](self: Pull[F, Int, R])
      def slidingMean(n: Int): Pull[F, Double, R] =
        def go(window: collection.immutable.Queue[Int], p: Pull[F, Int, R]): Pull[F, Double, R] =
          p.uncons.flatMap {
            case Left(r) => Result(r)
            case Right((hd, tl)) =>
              val newWindow = if window.size < n then window :+ hd else window.tail :+ hd
              val meanOfNewWindow = newWindow.sum / newWindow.size.toDouble
              Output(meanOfNewWindow) >> go(newWindow, tl)
          }
        go(collection.immutable.Queue.empty, self)

    given [F[_], O]: Monad[[x] =>> Pull[F, O, x]] with
      def unit[A](a: => A): Pull[F, O, A] = Result(a)
      extension [A](pa: Pull[F, O, A])
        def flatMap[B](f: A => Pull[F, O, B]): Pull[F, O, B] =
          pa.flatMap(f)

    extension [F[_], O](self: Pull[F, O, Unit])
      def flatMapOutput[O2](f: O => Pull[F, O2, Unit]): Pull[F, O2, Unit] =
        self.uncons.flatMap {
          case Left(()) => Result(())
          case Right((hd, tl)) =>
            f(hd) >> tl.flatMapOutput(f)
        }

    extension [F[_], O](self: Pull[F, O, Unit])
      def toStream: Stream[F, O] = self

  end Pull

  opaque type Stream[+F[_], +O] = Pull[F, O, Unit]
  object Stream:
    def empty: Stream[Nothing1, Nothing] = Pull.done

    def apply[O](os: O*): Stream[Nothing1, O] =
      fromList(os.toList)

    def fromList[O](os: List[O]): Stream[Nothing1, O] =
      os match
        case Nil => Pull.done
        case hd :: tl => Pull.Output(hd) >> fromList(tl)

    def fromLazyList[O](os: LazyList[O]): Stream[Nothing1, O] =
      os match
        case LazyList() => Pull.done
        case hd #:: tl => Pull.Output(hd) >> fromLazyList(tl)

    def unfold[O, R](init: R)(f: R => Option[(O, R)]): Stream[Nothing1, O] =
      Pull.unfold(init)(r => f(r).toRight(r)).void

    def continually[O](o: O): Stream[Nothing1, O] =
      Pull.Output(o) >> continually(o)

    def iterate[O](initial: O)(f: O => O): Stream[Nothing1, O] =
      Pull.Output(initial) >> iterate(f(initial))(f)

    def eval[F[_], O](fo: F[O]): Stream[F, O] =
      Pull.Eval(fo).flatMap(Pull.Output(_))

    def unfoldEval[F[_], O, R](init: R)(f: R => F[Option[(O, R)]]): Stream[F, O] =
      Pull.Eval(f(init)).flatMap {
        case None => Stream.empty
        case Some((o, r)) => Pull.Output(o) ++ unfoldEval(r)(f)
      }

    def fromIterator[O](itr: Iterator[O]): Stream[Nothing1, O] =
      if itr.hasNext then Pull.Output(itr.next) >> fromIterator(itr) else Pull.done

    def raiseError[F[_], O](t: Throwable): Stream[F, O] = Pull.Error(t)

    extension [F[_], O](self: Stream[F, O])
      def toPull: Pull[F, O, Unit] = self

      def fold[A](init: A)(f: (A, O) => A)(using MonadThrow[F]): F[A] = 
        self.fold(init)(f).map(_(1))

      def toList(using MonadThrow[F]): F[List[O]] =
        self.toList

      def run(using MonadThrow[F]): F[Unit] =
        fold(())((_, _) => ()).map(_(1))

      def ++(that: => Stream[F, O]): Stream[F, O] =
        self >> that

      def repeat: Stream[F, O] =
        self.repeat

      def take(n: Int): Stream[F, O] =
        self.take(n).void

      def filter(p: O => Boolean): Stream[F, O] =
        self.filter(p)

      def mapEval[O2](f: O => F[O2]): Stream[F, O2] =
        self.flatMapOutput(o => Stream.eval(f(o)))

      def handleErrorWith(handler: Throwable => Stream[F, O]): Stream[F, O] =
        Pull.Handle(self, handler)

      def onComplete(that: => Stream[F, O]): Stream[F, O] =
        handleErrorWith(t => that ++ raiseError(t)) ++ that

      def drain: Stream[F, Nothing] =
        self.flatMapOutput(o => Stream.empty)

    extension [O](self: Stream[Nothing, O])
      def fold[A](init: A)(f: (A, O) => A): A = 
        (self: Stream[SyncTask, O]).fold(init)(f).resultOrThrow(1)

      def toList: List[O] =
        (self: Stream[SyncTask, O]).toList.resultOrThrow

    given [F[_]]: Monad[[x] =>> Stream[F, x]] with
      def unit[A](a: => A): Stream[F, A] = Pull.Output(a)
      extension [A](sa: Stream[F, A])
        def flatMap[B](f: A => Stream[F, B]): Stream[F, B] =
          sa.flatMapOutput(f)

  type Pipe[F[_], -I, +O] = Stream[F, I] => Stream[F, O]

end ErrorHandling

object ErrorHandlingExample:
  import fpinscala.answers.iomonad.Task
  import scala.io.Source
  import ErrorHandling.Stream

  def acquire(path: String): Task[Source] =
    Task(Source.fromFile(path))

  def use(source: Source): Stream[Task, Unit] =
    Stream.eval(Task(source.getLines))
      .flatMap(itr => Stream.fromIterator(itr))
      .mapEval(line => Task(println(line)))

  def release(source: Source): Task[Unit] =
    Task(source.close())

  val prg: Stream[Task, Unit] = 
    Stream.eval(acquire("build.sbt")).flatMap(resource =>
      use(resource).onComplete(Stream.eval(release(resource))))

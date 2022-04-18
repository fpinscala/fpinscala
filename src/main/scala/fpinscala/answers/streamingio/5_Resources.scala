package fpinscala.answers.streamingio

import fpinscala.answers.iomonad.{IO, Monad}
import fpinscala.answers.monoids.Monoid
import scala.util.{Success, Failure}
import java.util.concurrent.atomic.AtomicReference
import java.io.BufferedWriter

object Resources:

  type Nothing1[A] = Nothing

  final class Ref[F[_], A] private (
    underlying: AtomicReference[A],
    delay: [A] => (() => A) => F[A]
  ):
    def get: F[A] = delay(() => underlying.get)
    def set(a: A): F[Unit] = delay(() => underlying.set(a))
    def modify[B](f: A => (A, B)): F[B] = delay { () =>
      @annotation.tailrec
      def loop(): B =
        val oldA = underlying.get
        val (newA, result) = f(oldA)
        if underlying.compareAndSet(oldA, newA) then result else loop()
      loop()
    }

  object Ref:
    def apply[F[_], A](initial: A)(using F: Monad[F]): Ref[F, A] =
      new Ref(new AtomicReference[A](initial),
        [A] => (th: () => A) => F.unit(()).map(_ => th()))

  final class Id

  final class Scope[F[_]](parent: Option[Scope[F]], val id: Id, state: Ref[F, Scope.State[F]])(using F: MonadThrow[F]):
    import Scope.State

    def open(finalizer: F[Unit]): F[Scope[F]] =
      state.modify {
        case State.Open(myFinalizer, subscopes) =>
          val sub = new Scope(Some(this), new Id, Ref(State.Open(finalizer, Vector.empty)))
          State.Open(myFinalizer, subscopes :+ sub) -> F.unit(sub)
        case State.Closed() =>
          val next = parent match
            case None => F.raiseError(new RuntimeException("root scope already closed"))
            case Some(p) => p.open(finalizer)
          State.Closed() -> next
      }.flatten

    def close: F[Unit] =
      state.modify {
        case State.Open(finalizer, subscopes) =>
          val finalizers = (subscopes.reverseIterator.map(_.close) ++ Iterator(finalizer)).toList
          def go(rem: List[F[Unit]], error: Option[Throwable]): F[Unit] =
            rem match
              case Nil => error match
                case None => F.unit(())
                case Some(t) => F.raiseError(t)
              case hd :: tl => hd.attempt.flatMap(res => go(tl, error orElse res.toEither.swap.toOption))
          State.Closed() -> go(finalizers, None)
        case State.Closed() => State.Closed() -> F.unit(())
      }.flatten

    def findScope(target: Id): F[Option[Scope[F]]] =
      findThisOrSubScope(target).flatMap {
        case Some(s) => F.unit(Some(s))
        case None => parent match
          case Some(p) => p.findScope(target)
          case None => F.unit(None)
      }

    def findThisOrSubScope(target: Id): F[Option[Scope[F]]] =
      if id == target then F.unit(Some(this))
      else state.get.flatMap {
        case State.Open(_, subscopes) =>
          def go(rem: List[Scope[F]]): F[Option[Scope[F]]] =
            rem match
              case Nil => F.unit(None)
              case hd :: tl => hd.findThisOrSubScope(target).flatMap {
                case Some(s) => F.unit(Some(s))
                case None => go(tl)
              }
          go(subscopes.toList)
        case State.Closed() => F.unit(None)
      }

  object Scope:
    enum State[F[_]]:
      case Open(finalizer: F[Unit], subscopes: Vector[Scope[F]]) extends State[F]
      case Closed() extends State[F]

    def root[F[_]](using F: MonadThrow[F]): Scope[F[_]] =
      new Scope(None, new Id, Ref(State.Open(F.unit(()), Vector.empty)))

  enum StepResult[F[_], +O, +R]:
    case Done(scope: Scope[F], result: R)
    case Out(scope: Scope[F], head: O, tail: Pull[F, O, R])
    def toUnconsResult: Either[R, (O, Pull[F, O, R])] = this match
      case Done(_, result) => Left(result)
      case Out(_, head, tail) => Right((head, tail))

  enum Pull[+F[_], +O, +R]:
    case Result[+R](result: R) extends Pull[Nothing, Nothing, R]
    case Output[+O](value: O) extends Pull[Nothing, O, Unit]
    case Eval[+F[_], R](action: F[R]) extends Pull[F, Nothing, R]
    case FlatMap[+F[_], X, +O, +R](
      source: Pull[F, O, X], f: X => Pull[F, O, R]) extends Pull[F, O, R]
    case FlatMapOutput[+F[_], O, +O2](
      source: Pull[F, O, Unit], f: O => Pull[F, O2, Unit]
    ) extends Pull[F, O2, Unit]
    case Uncons(source: Pull[F, O, R])
      extends Pull[F, Nothing, Either[R, (O, Pull[F, O, R])]]
    case Handle(
      source: Pull[F, O, R], handler: Throwable => Pull[F, O, R])
      extends Pull[F, O, R]
    case Error(t: Throwable) extends Pull[Nothing, Nothing, Nothing]
    case OpenScope(
      source: Pull[F, O, R], finalizer: Option[F[Unit]]
    ) extends Pull[F, O, R]
    case WithScope(
      source: Pull[F, O, R], scopeId: Id, returnScope: Id
    ) extends Pull[F, O, R]

    def step[F2[x] >: F[x], O2 >: O, R2 >: R](scope: Scope[F2])(
      using F: MonadThrow[F2]
    ): F2[StepResult[F2, O2, R2]] =
      import StepResult.{Done, Out}
      this match
        case Result(r) => F.unit(Done(scope, r))
        case Output(o) => F.unit(Out(scope, o, Pull.done))
        case Eval(action) => action.map(r => Done(scope, r))
        case Uncons(source) =>
          source.step(scope).map(s =>
            Done(scope, s.toUnconsResult.asInstanceOf[R2]))
        case Handle(source, f) =>
          source match
            case Handle(s2, g) =>
              s2.handleErrorWith(x =>
                g(x).handleErrorWith(y => f(y))).step(scope)
            case other =>
              other.step(scope).map {
                case Out(scope, hd, tl) => Out(scope, hd, Handle(tl, f))
                case Done(scope, r) => Done(scope, r)
              }.handleErrorWith(t => f(t).step(scope))
        case Error(t) => F.raiseError(t)
        case FlatMap(source, f) => 
          source match
            case FlatMap(s2, g) =>
              s2.flatMap(x => g(x).flatMap(y => f(y))).step(scope)
            case other => other.step(scope).flatMap {
              case Done(scope, r) => f(r).step(scope)
              case Out(scope, hd, tl) =>
                F.unit(Out(scope, hd, tl.flatMap(f)))
            }
        case FlatMapOutput(source, f) =>
          source.step(scope).flatMap {
            case Done(scope, r) => F.unit(Done(scope, r))
            case Out(scope, hd, tl) =>
              f(hd).flatMap(_ => tl.flatMapOutput(f)).step(scope)
          }
        case OpenScope(source, finalizer) =>
          scope.open(finalizer.getOrElse(F.unit(()))).flatMap(subscope => 
            WithScope(source, subscope.id, scope.id).step(subscope))
        case WithScope(source, scopeId, returnScopeId) =>
          scope.findScope(scopeId)
            .map(_.map(_ -> true).getOrElse(scope -> false))
            .flatMap { case (newScope, closeAfterUse) =>
              source.step(newScope).attempt.flatMap {
                case Success(Out(scope, hd, tl)) =>
                  F.unit(Out(scope, hd,
                             WithScope(tl, scopeId, returnScopeId)))
                case Success(Done(outScope, r)) =>
                  scope.findScope(returnScopeId)
                    .map(_.getOrElse(outScope))
                    .flatMap { nextScope =>
                      scope.close.as(Done(nextScope, r))
                    }
                case Failure(t) =>
                  scope.close.flatMap(_ => F.raiseError(t))
              }
            }

    def fold[F2[x] >: F[x], R2 >: R, A](init: A)(f: (A, O) => A)(
      using F: MonadThrow[F2]
    ): F2[(R2, A)] = 
      val scope = Scope.root[F2]
      def go(scope: Scope[F2], p: Pull[F2, O, R2], acc: A): F2[(R2, A)] =
        p.step(scope).flatMap {
          case StepResult.Done(_, r) => F.unit((r, init))
          case StepResult.Out(newScope, hd, tl) =>
            go(newScope, tl, f(init, hd))
        }
      go(scope, this, init).attempt.flatMap(res =>
        scope.close.flatMap(_ =>
          res.fold(F.raiseError, F.unit)
        )
      )

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

    def handleErrorWith[F2[x] >: F[x], O2 >: O, R2 >: R](
      handler: Throwable => Pull[F2, O2, R2]
    ): Pull[F2, O2, R2] =
      Handle(this, handler)

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
        FlatMapOutput(self, f)

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

    def resource[F[_], R](acquire: F[R])(release: R => F[Unit]): Stream[F, R] =
      Pull.Eval(acquire).flatMap(r => Pull.OpenScope(Pull.Output(r), Some(release(r))))

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
        self.take(n).void.scope

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

      def scope: Stream[F, O] =
        Pull.OpenScope(self, None)

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

end Resources

object ResourcesExample:
  import fpinscala.answers.iomonad.Task
  import scala.io.Source
  import scala.util.chaining.scalaUtilChainingOps
  import Resources.{Stream, Pipe}

  def file(path: String): Stream[Task, Source] =
    Stream.resource(Task(Source.fromFile(path)))(s => Task({println("CLOSING"); s.close()}))

  def lines(path: String): Stream[Task, String] =
    file(path).flatMap(source =>
      Stream.eval(Task(source.getLines)).flatMap(Stream.fromIterator)
    )

  val printLines: Stream[Task, Unit] = 
    lines("build.sbt").mapEval(line => Task(println(line)))

  import java.nio.file.{Files, Paths}

  def fileWriter(path: String): Stream[Task, BufferedWriter] =
    Stream.resource(Task(Files.newBufferedWriter(Paths.get(path))))(w => Task(w.close()))

  def writeLines(path: String): Pipe[Task, String, Unit] =
    lines => fileWriter(path).flatMap(writer =>
      lines.mapEval(line => Task {
        writer.write(line)
        writer.newLine
      }))

  def toCelsius(fahrenheit: Double): Double =
    (5.0 / 9.0) * (fahrenheit - 32.0)

  def trimmed[F[_]]: Pipe[F, String, String] =
    src => src.map(_.trim)

  def nonEmpty[F[_]]: Pipe[F, String, String] =
    _.filter(_.nonEmpty)

  def nonComment[F[_]]: Pipe[F, String, String] =
    src => src.filter(_.charAt(0) != '#')

  def asDouble[F[_]]: Pipe[F, String, Double] =
    src => src.flatMap { s =>
      s.toDoubleOption match
        case Some(d) => Stream(d)
        case None => Stream()
    }

  def convertToCelsius[F[_]]: Pipe[F, Double, Double] =
    src => src.map(toCelsius)

  def toString[F[_], A]: Pipe[F, A, String] =
    _.map(_.toString)

  val conversion: Pipe[Task, String, String] =
    trimmed andThen 
    nonEmpty andThen 
    nonComment andThen 
    asDouble andThen 
    convertToCelsius andThen
    toString

  def convert(inputFile: String, outputFile: String): Task[Unit] = 
    lines(inputFile).pipe(conversion).pipe(writeLines(outputFile)).run

  def convertAll(inputFile: String, outputFile: String): Task[Unit] =
    lines(inputFile)
      .flatMap(lines).pipe(conversion).pipe(writeLines(outputFile)).run
    
  def convertMultisink(inputFile: String): Task[Unit] =
    lines(inputFile).flatMap(file =>
      lines(file).pipe(conversion).pipe(writeLines(file + ".celsius"))).run
    


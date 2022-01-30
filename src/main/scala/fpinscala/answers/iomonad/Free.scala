package fpinscala.answers.iomonad

// This file contains the final form of Free that's developed throughout the chapter.
// See DerivingIO.scala for the step by step creation of Free.

enum Free[+F[_], A]:
  case Return(a: A) extends Free[Nothing, A]
  case Suspend(s: F[A])
  case FlatMap[F[_], A, B](
    s: Free[F, A],
    f: A => Free[F, B]) extends Free[F, B]

  def flatMap[F2[x] >: F[x], B](f: A => Free[F2,B]): Free[F2,B] =
    FlatMap(this, f)

  def map[B](f: A => B): Free[F,B] =
    flatMap(a => Return(f(a)))

  def union[G[_]]: Free[[x] =>> F[x] | G[x], A] = this
  def covary[F2[x] >: F[x]]: Free[F2, A] = this

  def run[F2[x] >: F[x]](using F: Monad[F2]): F2[A] = step match
    case Return(a) => F.unit(a)
    case Suspend(fa) => fa
    case FlatMap(Suspend(fa), f) => fa.flatMap(a => f(a).run)
    case FlatMap(_, _) => sys.error("Impossible, since `step` eliminates these cases")

  @annotation.tailrec
  final def step: Free[F, A] = this match
    case FlatMap(FlatMap(fx, f), g) => fx.flatMap(x => f(x).flatMap(y => g(y).covary[F])).step
    case FlatMap(Return(x), f) => f(x).step
    case _ => this

  def runFree[G[_]](t: [x] => F[x] => G[x])(using G: Monad[G]): G[A] =
    step match
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => t(r).flatMap[A](a => f(a).covary[F].runFree[G](t))
      case FlatMap(_, _) => sys.error("Impossible, since `step` eliminates these cases")

  def translate[G[_]](fToG: [x] => F[x] => G[x]): Free[G, A] =
    runFree([x] => (fx: F[x]) => Suspend(fToG(fx)))

object Free:
  given freeMonad[F[_]]: Monad[[x] =>> Free[F, x]] with
    def unit[A](a: => A) = Return(a)
    extension [A](fa: Free[F, A])
      def flatMap[B](f: A => Free[F, B]) = fa.flatMap(f)

  extension [A](fa: Free[Function0, A])
    @annotation.tailrec
    def runTrampoline: A = fa match
      case Return(a) => a
      case Suspend(ta) => ta()
      case FlatMap(fx, f) => fx match
        case Return(x) => f(x).runTrampoline
        case Suspend(tx) => f(tx()).runTrampoline
        case FlatMap(fy, g) => fy.flatMap(y => g(y).flatMap(f)).runTrampoline

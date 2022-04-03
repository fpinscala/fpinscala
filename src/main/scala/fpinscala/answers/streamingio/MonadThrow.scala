package fpinscala.answers.streamingio

import scala.util.{Try, Success, Failure}
import fpinscala.answers.iomonad.*

/*
 * A context in which exceptions can be caught and thrown.
 */
trait MonadThrow[F[_]] extends Monad[F]:
  extension [A](fa: F[A])
    def attempt: F[Try[A]]
    def handleErrorWith(h: Throwable => F[A]): F[A] =
      attempt.flatMap {
        case Failure(t) => h(t)
        case Success(a) => unit(a)
      }

  def raiseError[A](t: Throwable): F[A]

object MonadThrow:
  given taskInstance: MonadThrow[Task] with
    def unit[A](a: => A): Task[A] = Task(a)
    extension [A](fa: Task[A])
      def flatMap[B](f: A => Task[B]): Task[B] = Task.monad.flatMap(fa)(f)
      def attempt: Task[Try[A]] = Task.attempt(fa)
    def raiseError[A](err: Throwable): Task[A] = Task.raiseError(err)

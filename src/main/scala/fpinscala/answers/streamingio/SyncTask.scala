package fpinscala.answers.streamingio

import scala.util.{Try, Success, Failure}
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

opaque type SyncTask[A] = TailRec[Try[A]]

object SyncTask:
  extension [A](self: SyncTask[A])
    def result: Try[A] = self.result
    def resultOrThrow: A = self.result.get

  given monadThrowInstance: MonadThrow[SyncTask] with
    def unit[A](a: => A): SyncTask[A] =
      TailCalls.done(Try(a))

    def raiseError[A](t: Throwable): SyncTask[A] =
      TailCalls.done(Failure(t))

    extension [A](self: SyncTask[A])
      def flatMap[B](f: A => SyncTask[B]): SyncTask[B] =
        self.flatMap {
          case Success(a) => f(a)
          case Failure(t) => TailCalls.done(Failure(t))
        }

      def attempt: SyncTask[Try[A]] =
        self.map(t => Success(t))
    
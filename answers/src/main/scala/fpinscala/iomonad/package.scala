package fpinscala

package object iomonad {
  type IO[F[_],A] = IO3.IO[F,A]
  val IO = IO3.IO
  type Future[+A] = IO3.Future[A]
  val Future = IO3.Future
  type Trampoline[+A] = IO2.Trampoline[A]
  val Trampoline = IO2.Trampoline
}

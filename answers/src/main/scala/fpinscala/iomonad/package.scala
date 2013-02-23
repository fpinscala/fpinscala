package fpinscala

package object iomonad {
  type IO[F[_],A] = IO3.IO[F,A]
  val IO = IO3.IO
}

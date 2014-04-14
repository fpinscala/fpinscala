package fpinscala.iomonad

/**
 * A version of `TailRec` implemented using exceptions.
 * In the implementation of `flatMap`, rather than calling
 * the function, we throw an exception indicating what
 * function we want to call. A central loop repeatedly tries
 * and catches these exceptions to force the computation.
 */
trait Throw[+A] {
  import Throw._

  @annotation.tailrec
  final def run: A = this match {
    case Done(a) => a
    case More(thunk) => force(thunk).run
  }
}

object Throw extends Monad[Throw] {

  /* Exception indicating that the central loop should call `f(a)`. */
  case class Call[A,+B] private[Throw] (a: A, f: A => B) extends Exception {
    override def fillInStackTrace = this
  }

  case class Done[+A](a: A) extends Throw[A]
  case class More[+A](thunk: () => Throw[A]) extends Throw[A]

  /* Defer evaluation of `f(a)` to the central evaluation loop. */
  def defer[A,B](a: A)(f: A => B): B =
    throw new Call(a, f)

  /* Central evaluation loop. */
  def ap[A,B](a: A)(f: A => B): B = {
    var ai: Any = a
    var fi: Any => Any = f.asInstanceOf[Any => Any]
    while (true) {
      try return fi(ai).asInstanceOf[B]
      catch { case Call(a2,f2) => ai = a2; fi = f2; }
    }
    return null.asInstanceOf[B] // unreachable
  }

  /* Convenience function for forcing a thunk. */
  def force[A](f: () => A): A =
    ap(f)(f => f())

  def more[A](a: => Throw[A]): Throw[A] = More(() => a)

  /* `Throw` forms a `Monad`. */

  def unit[A](a: => A): Throw[A] = more(Done(a))

  def flatMap[A,B](a: Throw[A])(f: A => Throw[B]): Throw[B] =
    a match {
      case Done(a) => f(a)
      case More(thunk) =>
        try thunk() flatMap f
        catch { case Call(a0,g) => more {
          defer(a0)(g.asInstanceOf[Any => Throw[A]].
                    andThen(_ flatMap f))
        }}
    }
}

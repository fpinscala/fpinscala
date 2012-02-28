package fpinscala.exercises

// hide Scala's existing Stream type
import scala.{Stream => _} 

object Stream {
  def empty[A]: Stream[A] = 
    new Stream[A] {
      def uncons = None
    }
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = 
    new Stream[A] {
      lazy val uncons = Some((hd, tl)) 
    }
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

trait Stream[A] {
  def uncons: Option[(A, Stream[A])]

  def toList: List[A] = 
    sys.error("todo")

  def take(n: Int): Stream[A] = 
    sys.error("todo")

  def takeWhile(f: A => Boolean): Stream[A] = 
    sys.error("todo")
}

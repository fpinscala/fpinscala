package fpinscala.examples

// hide builtin scala Stream type
import scala.{Stream => _}

trait Stream[A] {
  def uncons: Option[(A, Stream[A])]
}

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

object StreamExamples {
  lazy val ones: Stream[Int] = Stream.cons(1, ones)
} 


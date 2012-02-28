package fpinscala.answers

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

  def toList: List[A] = { 
    @annotation.tailrec
    def go(acc: List[A], cur: Stream[A]): List[A] = 
      cur uncons match { 
        case Some((h,t)) => go(h :: acc, t)
        case _ => acc.reverse
      }
    go(List(), this)
  }

  def take(n: Int): Stream[A] = 
    if (n <= 0) Stream()
    else uncons.map { 
      case (h,t) => Stream.cons(h, t.take(n-1)) }.
      getOrElse (Stream.empty)

  def takeWhile(f: A => Boolean): Stream[A] = 
    uncons.map { 
      case (h,t) => 
        if (f(h)) Stream.cons(h,t.takeWhile(f)) 
        else Stream.empty[A] }.
      getOrElse (Stream.empty)
}

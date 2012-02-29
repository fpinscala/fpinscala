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

  def constant[A](a: A): Stream[A] = 
    sys.error("todo")

  def from(i: Int): Stream[Int] = 
    sys.error("todo")

  def fibs: Stream[Int] = 
    sys.error("todo")

  def unfold[A,S](s: S)(f: S => Option[(A,S)]): Stream[A] = 
    sys.error("todo")
}

trait Stream[A] {
  def uncons: Option[(A, Stream[A])]

  def toList: List[A] = 
    sys.error("todo")

  def take(n: Int): Stream[A] = 
    sys.error("todo")

  def takeWhile(f: A => Boolean): Stream[A] = 
    sys.error("todo")

  def foldRight[B](z: => B)(f: (A, => B) => B): B = 
    sys.error("todo")

  def ++(s2: Stream[A]): Stream[A] = 
    sys.error("todo")

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    sys.error("todo")

  def filter(f: A => Boolean): Stream[A] = 
    sys.error("todo")

  def exists(f: A => Boolean): Boolean = 
    sys.error("todo")

  def forall(f: A => Boolean): Boolean = 
    sys.error("todo")

  def takeWhile2(f: A => Boolean): Stream[A] = 
    sys.error("todo")

  def map[B](f: A => B): Stream[B] = 
    sys.error("todo")

  def map2[B](f: A => B): Stream[B] = 
    sys.error("todo")

  // (re)write `map`, `take`, `takeWhile`, `zip`, `zipAll`
    sys.error("todo")

  def startsWith(s: Stream[A]): Boolean = 
    sys.error("todo")

  def tails: Stream[Stream[A]] = 
    sys.error("todo")

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = 
    sys.error("todo")


}

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

  lazy val ones: Stream[Int] = 
    cons(1, ones) 

  def constant[A](a: A): Stream[A] = 
    Stream.cons(a, constant(a)) 

  def from(i: Int): Stream[Int] = 
    Stream.cons(i, from(i+1)) 

  def fibs: Stream[Int] = { 
    def go(lag1: Int, lag2: Int): Stream[Int] =
      Stream.cons(lag1, go(lag2, lag1+lag2))
    go(0, 1) 
  }

  def unfold[A,S](s: S)(f: S => Option[(A,S)]): Stream[A] = { 
    f(s).map { case (h,t) => Stream.cons(h, unfold(t)(f)) }.
         getOrElse(Stream.empty) 
  }
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

  def foldRight[B](z: => B)(f: (A, => B) => B): B = 
    uncons.
      map { case (h,t) => f(h, t.foldRight(z)(f)) }.
      getOrElse(z) 

  def ++(s2: Stream[A]): Stream[A] = 
    foldRight(s2)((h,t) => Stream.cons(h,t)) 

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    map(f).foldRight(Stream.empty[B])((h,t) => h ++ t) 

  def filter(f: A => Boolean): Stream[A] = 
    foldRight(this)((h,t) => if (f(h)) Stream.cons(h,t) else t) 

  def exists(f: A => Boolean): Boolean = 
    foldRight(false)((h,t) => f(h) || t) 

  def forall(f: A => Boolean): Boolean = 
    foldRight(true)((h,t) => f(h) && t) 

  def takeWhile2(f: A => Boolean): Stream[A] = 
    foldRight(Stream.empty[A]) { (h,t) => 
      if (f(h)) Stream.cons(h, t)
      else Stream.empty[A]
    } 

  def map[B](f: A => B): Stream[B] = 
    foldRight(Stream.empty[B])((h,t) => Stream.cons(f(h), t)) 

  def map2[B](f: A => B): Stream[B] = 
    Stream.unfold(this)(_.uncons map { case (h,t) => (f(h), t) }) 

  def take2(n: Int): Stream[A] = 
    Stream.unfold((this,n)) { case (s,i) => 
      if (i <= 0) None
      else s.uncons map { case (h,t) => (h,(t,n-1)) }
    } 

  def takeWhile3(f: A => Boolean): Stream[A] = 
    Stream.unfold(this)(_.uncons flatMap { case (h,t) =>  
      if (f(h)) Some((h,t))
      else None
    })

  def zip[B](s2: Stream[B]): Stream[(A,B)] = 
    Stream.unfold((this,s2)) { case (a,b) => for {
      as <- a.uncons; val (ha,ta) = as
      bs <- b.uncons; val (hb,tb) = bs
    } yield ((ha,hb), (ta,tb)) }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = 
    Stream.unfold((this,s2)) { case (a,b) => 
      (a.uncons, b.uncons) match {
        case (None, None) => None
        case (Some((h,t)), Some((h2,t2))) => 
          Some(((Some(h),Some(h2)), (t,t2))) 
        case (Some((h,t)), _) => Some(((Some(h),None), (t,b))) 
        case (_, Some((h,t))) => Some(((None,Some(h)), (a,t))) 
      }
    }

  def startsWith(s: Stream[A]): Boolean = 
    zipAll(s) takeWhile (_._2.isDefined) forall { case (a,b) => a == b }

  def tails: Stream[Stream[A]] = 
    Stream.unfold(this) { _.uncons map { 
      case (h,t) => (Stream.cons(h,t), t)
    }}

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = 
    foldRight((z, Stream(z))) { (a,s) =>
      val (z, acc) = s
      val z2 = f(a, z)
      (z2, Stream.cons(z2, acc))
    } . _2

  def hasSubsequence(s: Stream[A]): Boolean = 
    tails exists (_ startsWith s)
}

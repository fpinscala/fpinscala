package fpinscala.laziness

import Stream._

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

trait Stream[+A] {

  def headAndTail: Option[(A, Stream[A])] = this match {
    case Cons(h,t) => Some((h(),t()))
    case _         => None
  }
  

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = (n, this) match {
    case (0, _) => empty
    case (_, Empty) => empty
    case (k, Cons(h, t)) => cons(h(), t().take(k - 1))
  }

  def drop(n: Int): Stream[A] = (this, n) match {
    case (s,0)         => s
    case (Cons(h,t),m) => t().drop(m-1)
    case _             => Empty
  }
  

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhile_(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty: Stream[A]) { (a, b) =>
      if (p(a)) Stream.cons(a, b)
      else Stream.empty
    }

  def headOption: Option[A] =
    foldRight(None: Option[A]) { (a, b) =>
      Some(a)
    }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  //Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((a, b) =>
      cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A]) { (a, b) =>
      lazy val res = f(a)
      if (res) cons(a, b)
      else b
    }

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B]) { (a, b) =>
      f(a) append b
    }

  def zipWith[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    def toOption[C](c: C): Option[C] = Option(c)

    val a = this.map(toOption)
    val b =   s2.map(toOption)

    Stream.unfold((a,b)){ 
      case (Cons(ha,ta),Cons(hb,tb)) => Some(((ha() -> hb()) -> (ta() ->tb())))
      case _                         => None
     }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    def toOption[C](c: C): Option[C] = Option(c)

    val a = this.map(toOption).append(Stream.constant(None))
    val b =   s2.map(toOption).append(Stream.constant(None))

    Stream.unfold((a,b)){
      case (Cons(ha,ta),Cons(hb,tb)) 
              if ha() == None && hb() == None => None

      case (Cons(ha,ta),Cons(hb,tb))          => Some(((ha(),hb()),(ta(),tb())))
     }
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = {
    this.zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (a,b) => a == b
    }
  }

  //Exercise 5.15
  def tails: Stream[Stream[A]] = {
    Stream.unfold(this){
      case Empty  => None
      case s      => Some(s -> (s drop 1))
    } append empty
  }

  //Exercise 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((a,p) => {
      val b2 = f(a,p._1)
      (b2, cons(b2,p._2))
    })._2
  } 

}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  //Exercise 5.8
  def constant[A](k: A): Stream[A] = Stream.cons(k,constant(k))

  //Exercise 5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  //Exercise 5.10
  def fibStream: Stream[Int] = {
    def helper (a: Int,b: Int): Stream[Int] = cons(a, helper(b,a+b))
    helper(0,1)
  }

  //Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s1)) => cons(a,unfold(s1)(f))
    case _            => empty
  }

  //Exercise 5.12
  def fibsViaUnfold: Stream[Int] = {
    unfold((0,1))( c => Some((c._2,(c._2,c._2+c._1))))
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(a => Some((a,a+1)))
  }

  def onesViaUnfold: Stream[Int] = {
    unfold(1)(_ => Some((1,1)))
  }

  //Exercise 5.13
  def mapViaUnfold[A,B](a: Stream[A])(f: A => B): Stream[B]={
    unfold(a)(tmp => tmp.headAndTail.map(a1 => (f(a1._1),a1._2) ))
  }

   
}
package fpinscala.errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] = 
    map(f) getOrElse None
  
  /*
  Of course, we can also implement `flatMap` with explicit pattern matching.
  */
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  
  def orElse[B>:A](ob: Option[B]): Option[B] = 
    Some(this) getOrElse ob
  
  /*
  Again, we can implement this with explicit pattern matching. 
  */
  def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob 
    case _ => this
  }
  
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
  /*
  This can also be defined in terms of `flatMap`.
  */
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) None else Some(a))
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  case class MyException(msg: String) extends RuntimeException
  def failingFn(i: Int): Int = 
    try {
      if (i > 42) throw MyException("fail!")
      else i + 42
    } catch {
      case MyException(msg) => 42
    }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  import java.util.regex._
  
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  /* 
  The for-comprehension syntax is somewhat clearer. Here are both versions: 
  */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa =>
    b map     (bb =>
    f(aa, bb)))
  
  def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    for { 
      a1 <- a 
      b1 <- b 
    } yield f(a1,b1)

  def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((f,g) => f(s) && g(s))
  
  def mkMatcher(pat: String): Option[String => Boolean] = 
    pattern(pat) map (p => (s: String) => p.matcher(s).matches) // The details of this API don't matter too much, but `p.matcher(s).matches` will check if the string `s` matches the pattern `p`.

  /*
  Here is an explicit recursive version:
  */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h::t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  /*
  It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here, otherwise Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an unfortunate consequence of Scala using subtyping to encode algebraic data types.
  */
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  
  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))
}
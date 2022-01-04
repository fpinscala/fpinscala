package fpinscala.answers.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case None => None
    case Some(a) => Some(f(a))

  def getOrElse[B>:A](default: => B): B = this match
    case None => default
    case Some(a) => a

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  /* Of course, we can also implement `flatMap` with explicit pattern matching. */
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match
    case None => None
    case Some(a) => f(a)

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  /* Again, we can implement this with explicit pattern matching. */
  def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match
    case None => ob
    case _ => this

  def filter(f: A => Boolean): Option[A] = this match
    case Some(a) if f(a) => this
    case _ => None

  /* This can also be defined in terms of `flatMap`. */
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then Some(a) else None)

object Option:

  def failingFn(i: Int): Int =
    // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    val y: Int = throw new Exception("fail!")
    try
      val x = 42 + 5
      x + y
    // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern
    // that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
    catch  case e: Exception => 43

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      x + ((throw new Exception("fail!")): Int)
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // a bit later in the chapter we'll learn nicer syntax for
  // writing functions like this
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  /* Here's an explicit recursive version: */
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as match
      case Nil => Some(Nil)
      case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))

  /*
  It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
  Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an
  unfortunate consequence of Scala using subtyping to encode algebraic data types.
  */
  def sequence_1[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

package fpinscala.answers.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E, +A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
   
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(e) => Left(e)
    case Right(a) => f(a)

  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match
    case Left(_) => b
    case Right(a) => Right(a)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    for
      a <- this
      b1 <- b 
    yield f(a,b1)

object Either:
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
    as match
      case Nil => Right(Nil)
      case h :: t => (f(h).map2(traverse(t)(f)))(_ :: _)
  
  def traverse_1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))
  
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
    traverse(es)(x => x)

object AccumulatingErrors:
  import Either.{Left, Right}

  case class Name private (value: String)
  object Name:
    def apply(name: String): Either[String, Name] =
      if name == "" || name == null then Left("Name is empty.")
      else Right(new Name(name))

  case class Age private (value: Int)
  object Age:
    def apply(age: Int): Either[String, Age] =
      if age < 0 then Left("Age is out of range.")
      else Right(new Age(age))

  case class Person(name: Name, age: Age)
  object Person:
    def make(name: String, age: Int): Either[String, Person] =
      Name(name).map2(Age(age))(Person(_, _))

  def map2Both[E, A, B, C](
    a: Either[E, A],
    b: Either[E, B],
    f: (A, B) => C
  ): Either[List[E], C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(e), Right(_)) => Left(List(e))
      case (Right(_), Left(e)) => Left(List(e))
      case (Left(e1), Left(e2)) => Left(List(e1, e2))

  def makeBoth(name: String, age: Int): Either[List[String], Person] =
    map2Both(Name(name), Age(age), Person(_, _))

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] = 
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(es), Right(_)) => Left(es)
      case (Right(_), Left(es)) => Left(es)
      case (Left(es1), Left(es2)) => Left(es1 ++ es2)

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] = 
    as.foldRight(Right(Nil): Either[List[E], List[B]])((a, acc) => map2All(f(a), acc, _ :: _))

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] = 
    traverseAll(as, identity)

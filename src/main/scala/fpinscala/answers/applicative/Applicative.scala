package fpinscala.answers.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.monoids.Monoid
import fpinscala.answers.state.State

import java.time.LocalDate

trait Applicative[F[_]] extends Functor[F]:
  self =>

  def unit[A](a: => A): F[A]

  // We simply use `map2` to lift a function into `F` so we can apply it
  // to both `fab` and `fa`. The function being lifted here is `_(_)`,
  // which is the same as the lambda notation `(f, x) => f(x)`. That is,
  // It's a function that takes two arguments:
  //   1. A function `f`
  //   2. An argument `x` to that function
  // and it simply applies `f` to `x`.
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    fab.map2(fa)(_(_))

  extension [A](fa: F[A])
    // `map2` is implemented by first currying `f` so we get a function
    // of type `A => B => C`. This is a function that takes `A` and returns
    // another function of type `B => C`. We could map `f.curried` over
    // `F[A]` but let's stick with just `apply` and `unit`. We can lift
    // `f.curried` in to `F` via `unit`, giving us `F[A => B => C]`. Then
    // we can use `apply` along with `F[A]` to get `F[B => C]`. Passing
    // that to `apply` along with the `F[B]` will give us the desired `F[C]`.
    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    def map[B](f: A => B): F[B] =
      apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => f(a).map2(acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  extension [A](fa: F[A])
    def product[B](fb: F[B]): F[(A, B)] =
      fa.map2(fb)((_, _))

    def map3[B, C, D](
      fb: F[B],
      fc: F[C]
    )(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

    def map4[B, C, D, E](
      fb: F[B],
      fc: F[C],
      fd: F[D]
    )(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def product[G[_]](G: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] = new:
    def unit[A](a: => A) = (self.unit(a), G.unit(a))
    override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
      (self.apply(fs(0))(p(0)), G.apply(fs(1))(p(1)))

  // Here we simply use `map2` to lift `apply` and `unit` themselves from one
  // Applicative into the other.
  // If `self` and `G` both satisfy the laws, then so does the composite.
  // The full proof can be found at
  // https://github.com/runarorama/sannanir/blob/master/Applicative.v
  def compose[G[_]](G: Applicative[G]): Applicative[[x] =>> F[G[x]]] = new:
    def unit[A](a: => A) = self.unit(G.unit(a))
    extension [A](fga: F[G[A]])
      override def map2[B, C](fgb: F[G[B]])(f: (A, B) => C) =
        self.map2(fga)(fgb)(G.map2(_)(_)(f))

  def sequenceMap[K, V](ofv: Map[K, F[V]]): F[Map[K, V]] =
    ofv.foldLeft(unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      acc.map2(fv)((m, v) => m + (k -> v))
    }

object Applicative:

  opaque type ZipList[+A] = LazyList[A]

  object ZipList:
    def fromLazyList[A](la: LazyList[A]): ZipList[A] = la
    extension [A](za: ZipList[A]) def toLazyList: LazyList[A] = za

    given zipListApplicative: Applicative[ZipList] with
      def unit[A](a: => A): ZipList[A] =
        LazyList.continually(a)
      extension [A](fa: ZipList[A])
        override def map2[B, C](fb: ZipList[B])(f: (A, B) => C) =
          fa.zip(fb).map(f.tupled)

  enum Validated[+E, +A]:
    case Valid(get: A) extends Validated[Nothing, A]
    case Invalid(error: E) extends Validated[E, Nothing]
  
  object Validated:
    given validatedApplicative[E: Monoid]: Applicative[Validated[E, _]] with
      def unit[A](a: => A) = Valid(a)
      extension [A](fa: Validated[E, A])
        override def map2[B, C](fb: Validated[E, B])(f: (A, B) => C) =
          (fa, fb) match
            case (Valid(a), Valid(b)) => Valid(f(a, b))
            case (Invalid(e1), Invalid(e2)) =>
              Invalid(summon[Monoid[E]].combine(e1, e2))
            case (e @ Invalid(_), _) => e
            case (_, e @ Invalid(_)) => e

  object WebFormExample:
    case class WebForm(name: String, birthdate: LocalDate, phoneNumber: String)

    def validName(name: String): Validated[List[String], String] =
      if name != "" then Validated.Valid(name)
      else Validated.Invalid(List("Name cannot be empty"))

    def validBirthdate(birthdate: String): Validated[List[String], LocalDate] =
      try Validated.Valid(LocalDate.parse(birthdate))
      catch case _: java.time.format.DateTimeParseException =>
        Validated.Invalid(List("Birthdate must be in the form yyyy-MM-dd"))

    def validPhone(phoneNumber: String): Validated[List[String], String] =
      if phoneNumber.matches("[0-9]{10}") then Validated.Valid(phoneNumber)
      else Validated.Invalid(List("Phone number must be 10 digits"))

    def validateWebForm(name: String,
                        birthdate: String,
                        phone: String): Validated[List[String], WebForm] =
      validName(name).map3(
        validBirthdate(birthdate),
        validPhone(phone)
      )(WebForm(_, _, _))

  object WebFormExampleWithNonEmptyList:
    trait Semigroup[A]:
      def combine(x: A, y: A): A
    trait Monoid[A] extends Semigroup[A]:
      def empty: A

    given validatedApplicative[E: Semigroup]: Applicative[Validated[E, _]] with
      import Validated.{Valid, Invalid}
      def unit[A](a: => A) = Valid(a)
      extension [A](fa: Validated[E, A])
        override def map2[B, C](fb: Validated[E, B])(f: (A, B) => C) =
          (fa, fb) match
            case (Valid(a), Valid(b)) => Valid(f(a, b))
            case (Invalid(e1), Invalid(e2)) =>
              Invalid(summon[Semigroup[E]].combine(e1, e2))
            case (e @ Invalid(_), _) => e
            case (_, e @ Invalid(_)) => e

    case class NonEmptyList[+A](head: A, tail: List[A]):
      def toList: List[A] = head :: tail

    object NonEmptyList:
      def apply[A](head: A, tail: A*): NonEmptyList[A] =
        NonEmptyList(head, tail.toList)
      given nelSemigroup[A]: Semigroup[NonEmptyList[A]] with
        def combine(x: NonEmptyList[A], y: NonEmptyList[A]) =
          NonEmptyList(x.head, x.tail ++ (y.head :: y.tail))

    case class WebForm(name: String, birthdate: LocalDate, phoneNumber: String)

    def validName(name: String): Validated[NonEmptyList[String], String] =
      if name != "" then Validated.Valid(name)
      else Validated.Invalid(NonEmptyList("Name cannot be empty"))

    def validBirthdate(birthdate: String): Validated[NonEmptyList[String], LocalDate] =
      try
        Validated.Valid(LocalDate.parse(birthdate))
      catch
        case _: java.time.format.DateTimeParseException =>
          Validated.Invalid(NonEmptyList("Birthdate must be in the form yyyy-MM-dd"))

    def validPhone(phoneNumber: String): Validated[NonEmptyList[String], String] =
      if phoneNumber.matches("[0-9]{10}") then Validated.Valid(phoneNumber)
      else Validated.Invalid(NonEmptyList("Phone number must be 10 digits"))

    def validateWebForm(name: String,
                        birthdate: String,
                        phone: String): Validated[NonEmptyList[String], WebForm] =
      validName(name).map3(
        validBirthdate(birthdate),
        validPhone(phone)
      )(WebForm(_, _, _))

  type Const[A, B] = A

  given monoidApplicative[M](using m: Monoid[M]): Applicative[Const[M, _]] with
    def unit[A](a: => A): M = m.empty
    override def apply[A, B](m1: M)(m2: M): M = m.combine(m1, m2)

  given optionMonad: Monad[Option] with
    def unit[A](a: => A): Option[A] = Some(a)
    extension [A](oa: Option[A])
      override def flatMap[B](f: A => Option[B]) = oa.flatMap(f)

  given eitherMonad[E]: Monad[Either[E, _]] with
    def unit[A](a: => A): Either[E, A] = Right(a)
    extension [A](eea: Either[E, A])
      override def flatMap[B](f: A => Either[E, B]) = eea match
        case Right(a) => f(a)
        case Left(b) => Left(b)

  given stateMonad[S]: Monad[State[S, _]] with
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    extension [A](st: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)
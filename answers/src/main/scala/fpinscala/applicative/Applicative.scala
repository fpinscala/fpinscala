package fpinscala
package applicative

import monads.Functor
import state.*
import State.*
import monoids.*
import language.higherKinds
import language.implicitConversions


trait Applicative[F[_]] extends Functor[F]:

  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    // `map2` is implemented by first currying `f` so we get a function
    // of type `A => B => C`. This is a function that takes `A` and returns
    // another function of type `B => C`. So if we map `f.curried` over an
    // `F[A]`, we get `F[B => C]`. Passing that to `apply` along with the
    // `F[B]` will give us the desired `F[C]`.
    def map2[B,C](fb: F[B])(f: (A, B) => C): F[C] =
      apply(fa.map(f.curried)).apply(fb)

    def map[B](f: A => B): F[B] =
      apply(unit(f))(fa)


  // We simply use `map2` to lift a function into `F` so we can apply it
  // to both `fab` and `fa`. The function being lifted here is `_(_)`,
  // which is the same as the lambda notation `(f, x) => f(x)`. That is,
  // It's a function that takes two arguments:
  //   1. A function `f`
  //   2. An argument `x` to that function
  // and it simply applies `f` to `x`.
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    fab.map2(fa)(_(_))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => f(a).map2(fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    fa.map2(fb)((_,_))

  def product[G[_]](G: Applicative[G]): Applicative[[X] =>> (F[X], G[X])] =
    val self = this
    new Applicative[[X] =>> (F[X], G[X])]:
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))

  // Here we simply use `map2` to lift `apply` and `unit` themselves from one
  // Applicative into the other.
  // If `self` and `G` both satisfy the laws, then so does the composite.
  // The full proof can be found at
  // https://github.com/runarorama/sannanir/blob/master/Applicative.v
  def compose[G[_]](G: Applicative[G]): Applicative[[X] =>> F[G[X]]] =
    val self = this
    new Applicative[[X] =>> F[G[X]]]:
      def unit[A](a: => A) = self.unit(G.unit(a))
      extension [A](fga: F[G[A]])
        override def map2[B,C](fgb: F[G[B]])(f: (A,B) => C) =
          self.map2(fga)(fgb)(G.map2(_)(_)(f))

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldLeft(unit(Map.empty[K,V])) { case (acc, (k, fv)) =>
      acc.map2(fv)((m, v) => m + (k -> v))
    }

enum Validation[+E, +A]:
  case Success(a: A)
  case Failure(head: E, tail: Vector[E]) extends Validation[E, Nothing]

object Applicative:
  val lazyListApplicative: Applicative[LazyList] = new:

    def unit[A](a: => A): LazyList[A] =
      LazyList.continually(a) // The infinite, constant stream

    extension [A](a: LazyList[A])
      override def map2[B, C](b: LazyList[B])( // Combine elements pointwise
                              f: (A, B) => C): LazyList[C] =
        a.zip(b).map(f.tupled)

  def validationApplicative[E]: Applicative[Validation[E, _]] =
    import Validation.{Success, Failure}
    new Applicative[Validation[E, _]] {
      def unit[A](a: => A) = Success(a)
      extension [A](fa: Validation[E, A])
        override def map2[B, C](fb: Validation[E, B])(f: (A, B) => C) =
          (fa, fb) match {
            case (Success(a), Success(b)) => Success(f(a, b))
            case (Failure(h1, t1), Failure(h2, t2)) =>
              Failure(h1, t1 ++ Vector(h2) ++ t2)
            case (e@Failure(_, _), _) => e
            case (_, e@Failure(_, _)) => e
          }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[Const[M, _]] =
    new Applicative[Const[M, _]] {
      def unit[A](a: => A): M = M.empty
      override def apply[A,B](m1: M)(m2: M): M = M.combine(m1, m2)
    }


trait Monad[F[_]] extends Applicative[F]:
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join
    override def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))
    override def map2[B,C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(f))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

  extension [A](ffa: F[F[A]])
    def join: F[A] = ffa.flatMap(identity)

object Monad:

  // Notice that in the case of a `Left`, flatMap does nothing.
  def eitherMonad[E]: Monad[Either[E, _]] =
    new Monad[Either[E, _]]:
      def unit[A](a: => A): Either[E, A] = Right(a)
      extension [A](eea: Either[E, A])
        override def flatMap[B](f: A => Either[E, B]) = eea match
          case Right(a) => f(a)
          case Left(b) => Left(b)

  def stateMonad[S] = new Monad[State[S, _]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    extension [A](st: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)
  }

  // Monad composition
  def composeM[G[_], H[_]](using G: Monad[G], H: Monad[H], T: Traverse[H]):
    Monad[[X] =>> G[H[X]]] = new Monad[[X] =>> G[H[X]]]:
      def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
      extension [A](gha: G[H[A]])
        override def flatMap[B](f: A => G[H[B]]): G[H[B]] =
          G.flatMap(gha)(ha => G.map(T.traverse(ha)(f))(H.join))

trait Traverse[F[_]] extends Functor[F] with Foldable[F]:
  self =>
  def traverse[M[_]:Applicative,A,B](fa: F[A])(f: A => M[B]): M[F[B]] =
    sequence(map(fa)(f))
  def sequence[M[_]:Applicative,A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A
  object Id:
    given idMonad: Monad[Id] with
      def unit[A](a: => A) = a
      extension [A](a: A)
        override def flatMap[B](f: A => B): B = f(a)

  extension [A](fa: F[A])
    def map[B](f: A => B): F[B] =
      traverse[Id, A, B](fa)(f)(using Id.idMonad)

  import Applicative.*

  extension [A](fa: F[A])
    override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      traverse[Const[B, _], A, Nothing](fa)(f)(using monoidApplicative(mb))

    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      mapAccum(fa, acc)((a, b) => ((), f(b, a)))._2

    override def toList: List[A] =
      mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[State[S, _], A, B](fa)(f)(using Monad.stateMonad)

  def zipWithIndex_[A](ta: F[A]): F[(A,Int)] =
    traverseS(ta)((a: A) => (for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i))).run(0)._1

  def toList_[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => (for {
      as <- get[List[A]] // Get the current state, the accumulated list.
      _  <- set(a :: as) // Add the current element and set the new list as the new state.
    } yield ())).run(Nil)._2.reverse

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  def zip[A,B](fa: F[A], fb: F[B]): F[(A, B)] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    })._1

  def zipL[A,B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    })._1

  def zipR[A,B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    (mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    })._1

  def fuse[M[_],N[_],A,B](fa: F[A])(f: A => M[B], g: A => N[B])
                         (implicit M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) =
    traverse[[X] =>> (M[X], N[X]), A, B](fa)(a => (f(a), g(a)))(using M.product(N))

  def compose[G[_]](implicit G: Traverse[G]): Traverse[[X] =>> F[G[X]]] =
    new Traverse[[X] =>> F[G[X]]] {
      override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse:
  val listTraverse = new Traverse[List] {
    override def traverse[M[_],A,B](as: List[A])(f: A => M[B])(using M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, fbs) => f(a).map2(fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[M[_],A,B](oa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      oa match {
        case Some(a) => M.map(f(a))(Some(_))
        case None    => M.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree]:
    override def traverse[M[_],A,B](ta: Tree[A])(f: A => M[B])(using M: Applicative[M]): M[Tree[B]] =
      f(ta.head).map2(listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))

  // An example of a Foldable that is not a functor
  case class Iteration[A](a: A, f: A => A, n: Int):
    def foldMap[B](g: A => B)(M: Monoid[B]): B =
      def iterate(n: Int, b: B, c: A): B =
        if (n <= 0) b else iterate(n-1, g(c), f(a))
      iterate(n, M.empty, a)

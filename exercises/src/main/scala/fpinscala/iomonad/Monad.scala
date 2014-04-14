package fpinscala.iomonad

import language.higherKinds // Disable warnings for type constructor polymorphism

trait Functor[F[_]] {
  def map[A,B](a: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](a: F[A])(f: A => F[B]): F[B]

  def map[A,B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => unit(f(a)))
  def map2[A,B,C](a: F[A], b: F[B])(f: (A,B) => C): F[C] =
    flatMap(a)(a => map(b)(b => f(a,b)))
  def sequence_[A](fs: Stream[F[A]]): F[Unit] = foreachM(fs)(skip)
  def sequence_[A](fs: F[A]*): F[Unit] = sequence_(fs.toStream)
  def replicateM[A](n: Int)(f: F[A]): F[List[A]] =
    Stream.fill(n)(f).foldRight(unit(List[A]()))(map2(_,_)(_ :: _))
  def replicateM_[A](n: Int)(f: F[A]): F[Unit] =
    foreachM(Stream.fill(n)(f))(skip)
  def as[A,B](a: F[A])(b: B): F[B] = map(a)(_ => b)
  def skip[A](a: F[A]): F[Unit] = as(a)(())
  def when[A](b: Boolean)(fa: => F[A]): F[Boolean] =
    if (b) as(fa)(true) else unit(false)
  def forever[A,B](a: F[A]): F[B] = {
    lazy val t: F[B] = forever(a)
    a flatMap (_ => t)
  }
  def while_(a: F[Boolean])(b: F[Unit]): F[Unit] = {
    lazy val t: F[Unit] = while_(a)(b)
    a flatMap (c => skip(when(c)(t)))
  }
  def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a1 <- a
    ok <- cond(a1)
    _ <- if (ok) doWhile(a)(cond) else unit(())
  } yield ()

  def foldM[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[B] =
    l match {
      case h #:: t => f(z,h) flatMap (z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }
  def foldM_[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[Unit] =
    skip { foldM(l)(z)(f) }
  def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] =
    foldM_(l)(())((u,a) => skip(f(a)))
  def seq[A,B,C](f: A => F[B])(g: B => F[C]): A => F[C] =
    f andThen (fb => flatMap(fb)(g))

  // syntax
  implicit def toMonadic[A](a: F[A]): Monadic[F,A] =
    new Monadic[F,A] { val F = Monad.this; def get = a }
}

trait Monadic[F[_],A] {
  val F: Monad[F]
  import F._
  def get: F[A]
  private val a = get
  def map[B](f: A => B): F[B] = F.map(a)(f)
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(a)(f)
  def **[B](b: F[B]) = F.map2(a,b)((_,_))
  def *>[B](b: F[B]) = F.map2(a,b)((_,b) => b)
  def map2[B,C](b: F[B])(f: (A,B) => C): F[C] = F.map2(a,b)(f)
  def as[B](b: B): F[B] = F.as(a)(b)
  def skip: F[Unit] = F.skip(a)
  def replicateM(n: Int) = F.replicateM(n)(a)
  def replicateM_(n: Int) = F.replicateM_(n)(a)
}


package fpinscala.iomonad

trait Monad[F[_]] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](a: F[A])(f: A => F[B]): F[B]

  def map[A,B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => unit(f(a)))
  def map2[A,B,C](a: F[A], b: F[B])(f: (A,B) => C): F[C] = 
    flatMap(a)(a => map(b)(b => f(a,b)))
  def sequence_[A](fs: Stream[F[A]]): F[Unit] = foreachM(fs)(skip)
  def replicateM[A](n: Int)(f: F[A]): F[List[A]] = 
    Stream.fill(n)(f).foldRight(unit(List[A]()))(map2(_,_)(_ :: _))
  def replicateM_[A](n: Int)(f: F[A]): F[Unit] = 
    foreachM(Stream.fill(n)(f))(skip)
  def as[A,B](a: F[A])(b: B): F[B] = map(a)(_ => b)
  def skip[A](a: F[A]): F[Unit] = as(a)(())
  def when[A](b: Boolean)(fa: F[A]): F[Unit] = 
    if (b) skip(fa) else unit(())
  def forever[A,B](a: F[A]): F[B] = {
    lazy val t: F[B] = forever(a)
    a flatMap (_ => t)
  }
  def while_(a: F[Boolean]): F[Unit] = {
    lazy val t: F[Unit] = while_(a)
    a flatMap (b => when(b)(t))
  }
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
  def map[B](f: A => B): F[B] = F.map(get)(f)
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(get)(f)
  def **[B](b: F[B]) = F.map2(a,b)((_,_))
  def *>[B](b: F[B]) = F.map2(a,b)((_,b) => b)
  def as[B](b: B): F[B] = F.as(a)(b)
  def skip: F[Unit] = F.skip(a)
}

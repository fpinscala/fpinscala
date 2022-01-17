package fpinscala.answers.iomonad

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]

    def map[B](f: A => B): F[B] = flatMap(a => unit(f(a)))

    def map2[B,C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

    def **[B](fb: F[B]): F[(A, B)] = map2(fb)((_, _))
    def *>[B](fb: F[B]): F[B] = map2(fb)((_, b) => b)

    def as[B](b: B): F[B] = map(_ => b)

    def void: F[Unit] = as(())

    def forever[B]: F[B] =
      lazy val t: F[B] = flatMap(_ => t)
      t

    def doWhile(cond: A => F[Boolean]): F[Unit] = for
      a <- fa
      ok <- cond(a)
      _ <- if ok then doWhile(cond) else unit(())
    yield ()

    def replicateM(n: Int): F[List[A]] =
      LazyList.fill(n)(fa).foldRight(unit(List[A]()))(_.map2(_)(_ :: _))

    def replicateM_(n: Int): F[Unit] =
      foreachM(LazyList.fill(n)(fa))(_.void)

  def sequence_[A](fs: LazyList[F[A]]): F[Unit] = foreachM(fs)(_.void)

  def sequence_[A](fs: F[A]*): F[Unit] = sequence_(fs.to(LazyList))

  def when[A](b: Boolean)(fa: => F[A]): F[Boolean] =
    if b then fa.as(true) else unit(false)

  def while_(fa: F[Boolean])(fb: F[Unit]): F[Unit] =
    lazy val t: F[Unit] = while_(fa)(fb)
    fa.flatMap(c => when(c)(t).void)

  def foldM[A, B](l: LazyList[A])(z: B)(f: (B, A) => F[B]): F[B] =
    l match
      case h #:: t => f(z, h).flatMap(z2 => foldM(t)(z2)(f))
      case _ => unit(z)

  def foldM_[A,B](l: LazyList[A])(z: B)(f: (B,A) => F[B]): F[Unit] =
    foldM(l)(z)(f).void

  def foreachM[A](l: LazyList[A])(f: A => F[Unit]): F[Unit] =
    foldM_(l)(())((u,a) => f(a).void)

  def seq[A,B,C](f: A => F[B])(g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)


object Monad:

  given function0Monad: Monad[Function0] with
    def unit[A](a: => A) = () => a
    extension [A](fa: Function0[A])
      def flatMap[B](f: A => Function0[B]) =
        () => f(fa())()

  import fpinscala.answers.parallelism.Nonblocking.Par
  given parMonad: Monad[Par] with
    def unit[A](a: => A) = Par.unit(a)
    extension [A](fa: Par[A])
      def flatMap[B](f: A => Par[B]) =
        Par.fork(fa.flatMap(f)) 
given optionMonad: Monad[Option] with
  def unit[A](a: => A) = Some(a)
  extension [A](fa: Option[A])
    override def flatMap[B](f: A => Option[B]) =
      fa.flatMap(f)

given listMonad: Monad[List] with
  def unit[A](a: => A) = List(a)
  extension [A](fa: List[A])
    override def flatMap[B](f: A => List[B]) =
      fa.flatMap(f)

given lazyListMonad: Monad[LazyList] with
  def unit[A](a: => A) = LazyList(a)
  extension [A](fa: LazyList[A])
    override def flatMap[B](f: A => LazyList[B]) =
      fa.flatMap(f)

given parMonad: Monad[Par] with
  def unit[A](a: => A) = Par.unit(a)
  extension [A](fa: Par[A])
    override def flatMap[B](f: A => Par[B]): Par[B] =
      Par.flatMap(fa)(f)

def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new:
  def unit[A](a: => A) = p.succeed(a)
  extension [A](fa: P[A])
    override def flatMap[B](f: A => P[B]): P[B] =
      p.flatMap(fa)(f)

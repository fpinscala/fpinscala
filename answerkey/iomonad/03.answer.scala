  // Exercise 3: Implement a `Free` interpreter which works for any `Monad`
  def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => F.flatMap(r)(a => run(a))
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a0 => run(a0.flatMap(f))
    case _ => sys.error("Impossible, since `step` eliminates these cases")
  }

  def step[A](a: Free[F, A]): Free[F, A] = {
    a match {
      case FlatMap(Return(a), f) => step(f(a))
      case FlatMap(FlatMap(a, g), f) => step(a.flatMap(a0 => g(a0) flatMap f))
      case _ => a
    }
  }

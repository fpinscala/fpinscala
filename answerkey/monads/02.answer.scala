type IntState[A] = State[Int, A]

given stateIntMonad: Monad[StateInt] with
  def unit[A](a: => A) = State(s => (a, s))
  extension [A](fa: StateInt[A])
    override def flatMap[B](f: A => StateInt[B]) =
      State.flatMap(fa)(f)

// We can define a monad for any arbitrary S using a type lambda,
// which we'll learn about later in this chapter
given stateMonad[S]: Monad[[X] =>> State[S, X]] with
  def unit[A](a: => A): State[S,A] = State(s => (a, s))
  extension [A](st: State[S, A])
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State.flatMap(st)(f)
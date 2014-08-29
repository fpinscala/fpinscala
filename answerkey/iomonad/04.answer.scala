  def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = {
    type FreeG[A] = Free[G,A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G,A] = Suspend { fg(a) }
    }
    runFree(f)(t)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console,A]): A =
    runTrampoline { translate(a)(new (Console ~> Function0) {
      def apply[A](c: Console[A]) = c.toThunk
    })}


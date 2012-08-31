def modify[S](f: S => S): State[S, Unit] = for {
  s <- getState // Gets the current state and assigns it to `s`.
  _ <- setState(f(s)) // Sets the new state to `f` applied to `s`.
} yield ()
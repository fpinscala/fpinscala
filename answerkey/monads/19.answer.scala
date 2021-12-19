// Getting and setting the same state does nothing:
State.get.flatMap(State.set) == unit(())

// written as for-comprehension:
for
  x <- State.get
  _ <- State.set(x)
yield ()

// Setting the state to `s` and getting it back out yields `s`.
State.set(s).flatMap(_ => State.get) == unit(s)

// alternatively:
for
  _ <- State.set(s)
  x <- State.get
yield x
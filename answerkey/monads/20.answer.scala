// Getting and setting the same state does nothing:
getState.flatMap(setState) == unit(())

// written as for-comprehension:
for {
  x <- getState
  _ <- setState(x)
} yield ()

// Setting the state to `s` and getting it back out yields `s`.
setState(s).flatMap(_ => getState) == unit(s)

// alternatively:
for {
  _ <- setState(s)
  x <- getState
} yield x
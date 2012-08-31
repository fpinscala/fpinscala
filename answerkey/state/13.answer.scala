def getState[S]: State[S, S] =
  State(s => (s, s))

def setState[S](s: S): State[S, Unit] =
  State(_ => ((), s))
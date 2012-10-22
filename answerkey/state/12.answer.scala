def get[S]: State[S, S] =
  State(s => (s, s))

def set[S](s: S): State[S, Unit] =
  State(_ => ((), s))
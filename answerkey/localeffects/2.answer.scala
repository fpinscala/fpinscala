// An action that does nothing
def noop[S] = ST[S,Unit](())

def partition[S](a: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] = for {
  vp <- a.read(pivot)
  _ <- a.swap(pivot, r)
  j <- STRef(l)
  _ <- (l until r).foldLeft(noop[S])((s, i) => for {
    _ <- s
    vi <- a.read(i)
    _  <- if (vi < vp) (for {
      vj <- j.read
      _  <- a.swap(i, vj)
      _  <- j.write(vj + 1)
    } yield ()) else noop[S]
  } yield ())
  x <- j.read
  _ <- a.swap(x, r)
} yield x

def qs[S](a: STArray[S,Int], l: Int, r: Int): ST[S, Unit] = if (l < r) for {
  pi <- partition(a, l, r, l + (r - l) / 2)
  _ <- qs(a, l, pi - 1)
  _ <- qs(a, pi + 1, r)
} yield () else noop[S]
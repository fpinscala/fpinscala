def fill(xs: Map[Int,A]): ST[S,Unit] =
  xs.foldRight(ST[S,Unit](())) {
    case ((k, v), st) => st flatMap (_ => write(k, v))
  }
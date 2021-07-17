def map[B](f: A => B): Tree[B] = this match
  case Leaf(a) => Leaf(f(a))
  case Branch(l, r) => Branch(l.map(f), r.map(f))
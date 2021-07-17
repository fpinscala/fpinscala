/*
Again, note how similar the implementation is to `size` and `maximum`.
*/
def depth: Int = this match
  case Leaf(_) => 0
  case Branch(l, r) => 1 + (l.depth.max(r.depth))
def size: Int = this match
  case Leaf(_) => 1
  case Branch(l, r) => 1 + l.size + r.size
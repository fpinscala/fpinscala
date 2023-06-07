/*
The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
*/
def tails: Stream[Stream[A]] =
  cons(
    this,
    unfold(this) {
      case Empty => None
      case Cons(_, t) => Some(t(), t())
    }
  )

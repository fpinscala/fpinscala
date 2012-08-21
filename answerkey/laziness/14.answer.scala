/*
The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
*/
def tails: Stream[Stream[A]] = 
  unfold(this)(s => s.uncons match {
    case None => None 
    case Some((_,t)) => Some((s,t))
  }) append (Stream(empty))
/*
 * Exercise 5: Implement `|>`. Let the types guide your implementation.
 */
def |>[O2](p2: Process[O,O2]): Process[I,O2] = {
  p2 match {
    case Halt() => Halt()
    case Emit(h,t) => Emit(h, this |> t)
    case Await(f) => this match {
      case Emit(h,t) => t |> f(Some(h))
      case Halt() => Halt() |> f(None)
      case Await(g) => Await((i: Option[I]) => g(i) |> p2)
    }
  }
}

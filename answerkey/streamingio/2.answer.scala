/*
 * Exercise 2: Implement `count`.
 *
 * Here's one implementation, with three stages - we map all inputs
 * to 1.0, compute a running sum, then finally convert the output
 * back to `Int`. The three stages will be interleaved - as soon
 * as the first element is examined, it will be converted to 1.0,
 * then added to the running total, and then this running total
 * will be converted back to `Int`, then the `Process` will examine
 * the next element, and so on.
 */
def count[I]: Process[I,Int] =
  lift((i: I) => 1.0) |> sum |> lift(_.toInt)

/* For comparison, here is an explicit recursive implementation. */
def count2[I]: Process[I,Int] = {
  def go(n: Int): Process[I,Int] =
    await((i: I) => emit(n+1, go(n+1)))
  go(0)
}


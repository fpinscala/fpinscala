/*
 * Exercise 8: Implement `exists`
 *
 * We choose to emit all intermediate values, and not halt.
 * See `existsResult` below for a trimmed version.
 */
def exists[I](f: I => Boolean): Process[I,Boolean] =
  lift(f) |> any

/* Emits whether a `true` input has ever been received. */
def any: Process[Boolean,Boolean] =
  loop(false)((b:Boolean,s) => (s || b, s || b))

/* A trimmed `exists`, containing just the final result. */
def existsResult[I](f: I => Boolean) =
  exists(f) |> takeThrough(!_) |> dropWhile(!_) |> echo.orElse(emit(false))

/*
 * Like `takeWhile`, but includes the first element that tests
 * false.
 */
def takeThrough[I](f: I => Boolean): Process[I,I] =
  takeWhile(f) ++ echo

/* Awaits then emits a single value, then halts. */
def echo[I]: Process[I,I] = await(i => emit(i))

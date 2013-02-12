object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      Reader(r => f(st.run(r)).run(r))
  }

  // A primitive operation for it would be simply to ask for the `R` argument:
  def ask[R]: Reader[R, R] = Reader(r => r)
}

// The action of Reader's `flatMap` is to pass the `r` argument along to both the
// outer Reader and also to the result of `f`, the inner Reader. Similar to how
// `State` passes along a state, except that in `Reader` the "state" is read-only.

// The meaning of `sequence` here is that if you have a list of functions, you can
// turn it into a function that takes one argument and passes it to all the functions
// in the list, returning a list of the results.

// The meaning of `join` is simply to pass the same value as both arguments to a
// binary function.

// The meaning of `replicateM` is to apply the same function a number of times to
// the same argument, returning a list of the results. Note that if this function
// is _pure_, (which it should be), this can be exploited by only applying the
// function once and replicating the result instead of calling the function many times.
// This means the Reader monad can override replicateM to provide a very efficient
// implementation.
// We could introduce a combinator, `wrap`:

  def wrap[A](p: => Parser[A]): Parser[A]

// Then define `many` as:

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, wrap(many(p)))(_ :: _) or succeed(List())

// In the parallelism chapter, we were particularly interested in avoiding having `Par` objects that took as much time and space to build as the corresponding serial computation, and the `delay` combinator let us control this more carefully. Here, this isn't as much of a concern, and having to think carefully each time we `map2` to decide whether we need to call `wrap` seems like unnecessary friction for users of the API.

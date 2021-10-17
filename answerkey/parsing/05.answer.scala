// We could introduce a combinator, `defer`:

  def defer[A](p: => Parser[A]): Parser[A]

// Then define `many` as:

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, defer(many(p)))(_ :: _) or succeed(List())

// In the parallelism chapter, we were particularly interested in avoiding having `Par` objects that took as much time and space to build as the corresponding serial computation, and the `delay` combinator let us control this more carefully. Here, this isn't as much of a concern, and having to think carefully each time we `map2` to decide whether we need to call `defer` seems like unnecessary friction for users of the API.

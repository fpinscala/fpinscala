/*
There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors, a simple approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:

trait Partial[+A,+B]
case class Errors[+A](get: Seq[A]) extends Validation[A,Nothing]
case class Success[+B](get: B) extends Validation[Nothing,B]

You can implement `map`, `map2`, `sequence`, and so on for this type in such a way that errors are accumulated when possible. (This idea can even be generalized further - we don't need to accumulate failing values into a list, we can accumulate values using any user-supplied binary function)

It's also possible to use `Either[List[E],_]` directly to accumulate errors, using different implementations of helper functions like `map2` and `sequence`.
*/
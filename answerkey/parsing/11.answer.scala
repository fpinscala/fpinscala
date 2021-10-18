/** In the event of an error, returns the error that occurred after consuming the most number of characters. */
extension [A](p: Parser[A]) def furthest: Parser[A]

/** In the event of an error, returns the error that occurred most recently. */
extension [A](p: Parser[A]) def latest: Parser[A]

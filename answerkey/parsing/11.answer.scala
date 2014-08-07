/** In the event of an error, returns the error that occurred after consuming the most number of characters. */
def furthest[A](p: Parser[A]): Parser[A]

/** In the event of an error, returns the error that occurred most recently. */
def latest[A](p: Parser[A]): Parser[A]

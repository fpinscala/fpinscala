/*
 * Create a `Process[IO,O]` from the lines of a file, using
 * the `resource` combinator above to ensure the file is closed
 * when processing the stream of lines is finished.
 */
def lines(filename: String): Process[IO,String] =
  resource
    { IO(io.Source.fromFile(filename)) }
    { src =>
        lazy val iter = src.getLines // a stateful iterator
        def step = if (iter.hasNext) Some(iter.next) else None
        lazy val lines: Process[IO,String] = eval(IO(step)).flatMap {
          case None => Halt(End)
          case Some(line) => Emit(line, lines)
        }
        lines
    }
    { src => eval_ { IO(src.close) } }

/* Exercise 11: Implement `eval`, `eval_`, and use these to implement `lines`. */
def eval[F[_],A](a: F[A]): Process[F,A] =
  await[F,A,A](a) {
    case Left(err) => Halt(err)
    case Right(a) => Emit(a, Halt(End))
  }

/* Evaluate the action purely for its effects. */
def eval_[F[_],A,B](a: F[A]): Process[F,B] =
  eval[F,A](a).drain[B]


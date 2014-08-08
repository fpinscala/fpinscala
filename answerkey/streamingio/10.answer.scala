/*
 * Exercise 10: This function is defined only if given a `MonadCatch[F]`.
 * Unlike the simple `runLog` interpreter defined in the companion object
 * below, this is not tail recursive and responsibility for stack safety
 * is placed on the `Monad` instance.
 */
def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
  def go(cur: Process[F,O], acc: IndexedSeq[O]): F[IndexedSeq[O]] =
    cur match {
      case Emit(h,t) => go(t, acc :+ h)
      case Halt(End) => F.unit(acc)
      case Halt(err) => F.fail(err)
      case Await(req,recv) => F.flatMap (F.attempt(req)) { e => go(Try(recv(e)), acc) }
    }
  go(this, IndexedSeq())
}

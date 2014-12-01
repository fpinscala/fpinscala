/* This version respects timeouts. See `Map2Future` below. */
def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
  es => {
    val (af, bf) = (a(es), b(es))
    Map2Future(af, bf, f)
  }

/*
Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel. We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation of pure values won't affect results).
*/
case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                             f: (A,B) => C) extends Future[C] {
  @volatile var cache: Option[C] = None
  def isDone = cache.isDefined
  def isCancelled = a.isCancelled || b.isCancelled
  def cancel(evenIfRunning: Boolean) =
    a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
  def get = compute(Long.MaxValue)
  def get(timeout: Long, units: TimeUnit): C =
    compute(TimeUnit.MILLISECONDS.convert(timeout, units))

  private def compute(timeoutMs: Long): C = cache match {
    case Some(c) => c
    case None =>
      val start = System.currentTimeMillis
      val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
      val stop = System.currentTimeMillis; val at = stop-start
      val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS)
      val ret = f(ar, br)
      cache = Some(ret)
      ret
  }
}

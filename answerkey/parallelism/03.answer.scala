/* This version respects timeouts. 
   Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel. We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation of pure values won't affect results).
*/
extension [A](pa: Par[A]) def map2Timeouts[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
  es => new Future[C]:
    private val futureA = pa(es)
    private val futureB = pb(es)
    @volatile private var cache: Option[C] = None

    def isDone = cache.isDefined
    def get() = get(Long.MaxValue, TimeUnit.NANOSECONDS)

    def get(timeout: Long, units: TimeUnit) =
      val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, units)
      val started = System.nanoTime
      val a = futureA.get(timeoutNanos, TimeUnit.NANOSECONDS)
      val elapsed = System.nanoTime - started
      val b = futureB.get(timeoutNanos - elapsed, TimeUnit.NANOSECONDS)
      val c = f(a, b)
      cache = Some(c)
      c

    def isCancelled = futureA.isCancelled || futureB.isCancelled
    def cancel(evenIfRunning: Boolean) =
      futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)
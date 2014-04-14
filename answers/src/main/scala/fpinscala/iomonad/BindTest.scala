package fpinscala.iomonad

object BindTest extends App {

  def timeit(n: Int)(task: => Unit): Unit = {
    val start = System.currentTimeMillis
    (0 to n).foreach { _ => task }
    val stop = System.currentTimeMillis
    println((stop - start) / 1000.0 + " seconds")
  }

  val N = 100000
  def go[F[_]](F: Monad[F])(unit: F[Unit])(f: F[Int] => Int): Unit = {
    import F.toMonadic
    f { (0 to N).map(i => F.map(unit)(_ => i)).foldLeft(F.unit(0)) {
      (f1,f2) => for {
        acc <- f1
        i <- f2
      } yield { // if (i == N) println("result: " + (acc+i))
                (acc + i)
              }
    }}
  }

  import fpinscala.parallelism.Nonblocking._

  object ParMonad extends Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A,B](pa: Par[A])(f: A => Par[B]) = Par.fork { Par.flatMap(pa)(f) }
  }

  val pool = java.util.concurrent.Executors.newFixedThreadPool(4)

  timeit(10) { go(Throw)(Throw.unit(())) ( _ run ) }
  timeit(10) { go(IO2b.TailRec)(IO2b.TailRec.unit(())) ( IO2b.run ) }
  timeit(10) { go(IO2c.Async)(IO2c.Async.unit(()))(r => Par.run(pool) { IO2c.run(r) }) }
  timeit(10) { go[IO](ioMonad)(ioMonad.unit(()))(r => unsafePerformIO(r)(pool)) }
  timeit(10) { go(Task)(Task.now(()))(r => r.run(pool)) }
  timeit(10) { go(Task)(Task.forkUnit(()))(r => r.run(pool)) }
  timeit(10) { go(ParMonad)(ParMonad.unit(())) { p => Par.run(pool)(p) }}

  // Par.run(pool)(ParMonad.forever { ParMonad.unit { println("woot") }})
  pool.shutdown()
}

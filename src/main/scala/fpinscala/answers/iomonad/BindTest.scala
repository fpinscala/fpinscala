package fpinscala.answers.iomonad

object BindTest extends App {

  def timeit(n: Int)(task: => Unit): Unit = {
    val start = System.currentTimeMillis
    (0 to n).foreach { _ => task }
    val stop = System.currentTimeMillis
    println(s"${(stop - start) / 1000.0} seconds")
  }

  val N = 100000
  def go[F[_]](unit: F[Unit])(f: F[Int] => Int)(using F: Monad[F]): Unit = {
    f { (0 to N).map(i => unit.map(_ => i)).foldLeft(F.unit(0)) {
      (f1,f2) => for {
        acc <- f1
        i <- f2
      } yield { // if (i == N) println("result: " + (acc+i))
                (acc + i)
              }
    }}
  }

  import fpinscala.answers.parallelism.Nonblocking.*

  given parMonad: Monad[Par] with
    def unit[A](a: => A) = Par.unit(a)
    extension [A](fa: Par[A])
      def flatMap[B](f: A => Par[B]) = Par.fork(fa.flatMap(f))

  val pool = java.util.concurrent.Executors.newFixedThreadPool(4)

  timeit(10) { go(Throw(()))(_.run) }
  timeit(10) { go(IO2b.TailRec(()))(_.run) }
  timeit(10) { go(IO2c.Async(()))(_.run.run(pool)) }
  timeit(10) { go(summon[Monad[IO3.IO]].unit(()))(_.unsafeRunSync(using pool)) }
  timeit(10) { go(Task.now(()))(_.run(using pool)) }
  timeit(10) { go(Task.forkUnit(()))(_.run(using pool)) }
  timeit(10) { go(parMonad.unit(()))(_.run(pool))}

  // parMonad.unit(println("woot")).forever.run(pool)
  pool.shutdown()
}

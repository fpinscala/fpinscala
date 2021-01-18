package fpinscala.iomonad

import fpinscala.iomonad.ShalamaiIO.IO.unit
import fpinscala.iomonad.ShalamaiIO.{IO, PrintLine}

import scala.annotation.tailrec
import scala.io.StdIn

object ShalamaiIO extends App {

  trait IO[A] {
    def run: A
  }

  object IO extends Monad[IO] {
    override def unit[A](a: => A): IO[A] = new IO[A] {
      override def run: A = a
    }

    override def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] = new IO[B] {
      override def run: B = {
        val r = a.run
        val fm = f(r)
        fm.run
      }
    }

    def apply[A](a: => A): IO[A] = unit(a)
  }

  def ReadLine: IO[String] = IO {
    StdIn.readLine()
  }

  def PrintLine(msg: String): IO[Unit] = IO {
    println(msg)
  }


  def sumApp: IO[Unit] = for {
    _ <- PrintLine("yo, type some int")
    n1 <- ReadLine.map(_.toInt)
    _ <- PrintLine("and another one")
    n2 <- ReadLine.map(_.toInt)
    sum = n1 + n2
    _ <- PrintLine(s"here is the sum of them: $sum. Boom!!!")
  } yield ()

  //  sumApp.run

  def foreverPrint: IO[Unit] = IO.forever(PrintLine("yo yo yo ..."))

  //  foreverPrint.run

  val p = PrintLine("yo")
  val x: IO[Unit] = p.flatMap(_ => p.flatMap(_ => p.flatMap(_ => p)))
  x.run

  //  IO { 2 }
}

object ShalamaiIO2 extends App {

  sealed trait IO[A]

  case class Return[A](a: A) extends IO[A]

  case class Suspend[A](resume: () => A) extends IO[A]

  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  object IO extends Monad[IO] {
    override def unit[A](a: => A): IO[A] = Return(a)

    override def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] = FlatMap(a, f)

    def apply[A](a: => A): IO[A] = unit(a)

    @tailrec
    def run[A](io: IO[A]): A = io match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a))
        case Suspend(resume) => run(f(resume()))
        case FlatMap(y, g) => run(y.flatMap(v => g(v).flatMap(f)))
      }
    }
  }

  def PrintLine(msg: String): IO[Unit] = Suspend(() => println(msg))

  def foreverPrint: IO[Unit] = IO.forever(PrintLine("yo yo yo ..."))

  //  IO.run(Return(println("yo")))

  //  IO.run(foreverPrint)


  val f: Int => IO[Int] = (x: Int) => Return(x + 1)

  val g = List.fill(100000)(f).foldLeft(f) {
    (acc, b) => x => Suspend(() => x).flatMap(acc).flatMap(b)
  }

  val gr = g(42)
  println(IO.run(gr))
}

object ShalamaiFreeIO extends App {
  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](sub: Free[F, A], k: A => Free[F, B]) extends Free[F, B]

  def freeMonad[F[_]] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)

    override def flatMap[A, B](a: Free[F, A])(f: A => Free[F, B]): Free[F, B] = a.flatMap(f)
  }

  @tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(s) => runTrampoline(f(s()))
      case FlatMap(y, g) => runTrampoline(y.flatMap(res => g(res).flatMap(f)))
    }
  }


  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = {
    @tailrec
    def step(free: Free[F, A]): Free[F, A] = free match {
      case FlatMap(FlatMap(x, f), g) => step(x.flatMap(res => f(res).flatMap(g)))
      case FlatMap(Return(a), g) => step(g(a))
      case _ => free
    }

    step(a) match {
      case Return(a) => F.unit(a)
      case Suspend(s) => s
      case FlatMap(x, f) => x match {
        case Suspend(s) => F.flatMap(s)(res => run(f(res)))
        case _ => throw new Error("impossible ...")
      }
    }
  }

  val app = Return(5).flatMap(x => Suspend(() => x + 4))
  val res = runTrampoline(app)
  println(res)

  val res2 = run(app)(new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A, B](a: () => A)(f: A => () => B): () => B = f(a())
  })

  println(res2())

}
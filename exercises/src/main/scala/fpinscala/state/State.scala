package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, nxtState) => nonNegativeInt(nxtState)
    case (value, retState) if value < 0 => (-1 * value, retState)
    case ret => ret
  }

  @tailrec
  def double(rng: RNG): (Double, RNG) = {
    nonNegativeInt(rng) match {
      case (Int.MaxValue, s) => double(s)
      case (posInt, s) => (posInt/Int.MaxValue.toDouble, s)
    }
  }

  def doubleAsMap: Rand[Double] = {
    map(nonNegativeInt) { posInt =>
      posInt/(Int.MaxValue.toDouble + 1)
    }
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, s) = rng.nextInt
    val (d, s2) = double(s)
    ((i, d), s2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), s) = intDouble(rng)
    ((d, i), s)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d, s) = double(rng)
    val (d2, s2) = double(s)
    val (d3, s3) = double(s2)
    ((d, d2, d3), s3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, intList: List[Int], rng: RNG): (List[Int], RNG) = {
      if (count <= 0) {
        (intList, rng)
      } else {
        val (i, s) = rng.nextInt
        loop(count - 1, i :: intList, s)
      }
    }
    loop(count, List(), rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  def ints2(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(_.nextInt))
  }


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, s) = f(rng)
      g(a)(s)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + n - mod > 0) unit(mod) else nonNegativeLessThan(n)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

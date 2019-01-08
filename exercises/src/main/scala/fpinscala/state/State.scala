package fpinscala.state


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

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (i, r) if i == Int.MinValue => (0, r)
      case (i, r) if i < 0 => (math.abs(i), r)
      case (i, r) => (i, r)
    }

  def double(rng: RNG): (Double, RNG) =
    rng.nextInt match { case (i, r) => (math.abs((i.toDouble - 1d) / Int.MaxValue.toDouble), r) }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, r1) = rng.nextInt
    val (i2, r2) = double(r1)
    ((i1, i2), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i1, i2), r)= intDouble(rng)
    ((i2, i1), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (i1, r1) = double(rng)
    val (i2, r2) = double(r1)
    val (i3, r3) = double(r2)
    ((i1, i2, i3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) (Nil, rng)
    else
      rng.nextInt match { case (i, r) =>
        ints(count-1)(r) match { case (li, r2) =>
          (i :: li, r2)
        }
      }

  def doubleViaMap(rng: RNG): (Double, RNG) = map(int)(i => math.abs((i.toDouble - 1d) / Int.MaxValue.toDouble))(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
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

package fpinscala.state

import fpinscala.state.State.{machineInteraction, sequence}

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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    val positiveN = if (n == Int.MinValue) Int.MaxValue else Math.abs(n)
    (positiveN, rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    val doubleN = (n - 1).toDouble / Int.MaxValue
    (doubleN, rng2)
  }

  def _double: Rand[Double] = map(nonNegativeInt)(x => (x - 1).toDouble / Int.MaxValue)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (n, rng3) = rng2.nextInt
    ((d, n), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(randoms: List[Int], generator: RNG, n: Int): (List[Int], RNG) = {
      if (n <= 0) (randoms, generator)
      else {
        val (random, generator2) = generator.nextInt
        go(randoms :+ random, generator2, n - 1)
      }
    }

    go(Nil, rng, count)
  }

  def _ints(count: Int): Rand[List[Int]] = sequence(Seq.fill(count)(int).toList)

  def _ints2(count: Int): Rand[List[Int]] = sequence2(Seq.fill(count)(int).toList)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => fs.foldLeft[(List[A], RNG)]((Nil, rng))({ case ((acc, gen), head) =>
      val (a, nextGen) = head(gen)
      (acc :+ a, nextGen)
    })

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit[List[A]](Nil))((h, z) => map2(h, z)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def main(args: Array[String]): Unit = {
    println(nonNegativeInt(Simple(123)))
    println(double(Simple(123)))
    println(_double(Simple(123)))
    println(intDouble(Simple(123)))
    println(doubleInt(Simple(123)))
    println(double3(Simple(123)))
    println(ints(10)(Simple(123)))
    println(_ints(10)(Simple(123)))
    println(_ints2(10)(Simple(123)))
    println(nonNegativeLessThan((Int.MaxValue * 0.66).toInt)(Simple(239048)))
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](seq: List[State[S, A]]): State[S, List[A]] =
    seq.foldRight[State[S, List[A]]](unit(Nil))((h, acc) => h.map2(acc)(_ :: _))

  type Rand[A] = State[RNG, A]

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(machineInteraction))
    m <- get[Machine]
  } yield (m.coins, m.candies)

  def machineInteraction(input: Input): State[Machine, Unit] = modify[Machine] { m =>
    if (m.candies == 0) m
    else input match {
      case Coin => if (m.locked) Machine(locked = false, m.candies, m.coins + 1) else m
      case Turn => if (!m.locked) Machine(locked = true, m.candies - 1, m.coins) else m
    }
  }

  def main(args: Array[String]): Unit = {
    val inputs = Coin :: Turn :: Coin :: Turn :: Turn :: Coin :: Coin :: Turn :: Nil
    println(simulateMachine(inputs).run(Machine(locked = true, 10, 0)))
  }
}

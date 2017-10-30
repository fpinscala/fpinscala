package fpinscala.state

import fpinscala.state.State.{get, modify, sequence}


trait RNG {
  // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
  def nextInt: (Int, RNG)
}

object RNG {

  // NB - this was called SimpleRNG in the book text
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed)                                // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt                               // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG)                                                 // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  /**
    * Function composition on a state transition. Note that the state is unchanged; we apply `f` to
    * the return value of the state transition, and return the result, along with the new state
    * computed by the transition.
    */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  /** Method using `map`. */
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /**
    * Exercise 6.1
    *
    * Write a function that uses `RNG.nextInt` to generate a random integer between `0` and
    * `Int.maxValue` (inclusive). Make sure to handle the corner case when `nextInt` returns
    * `Int.MinValue`, which doesn't have a non-negative counterpart.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt

    (if (i < 0) -(i + 1) else i, r)
  }

  /**
    * Exercise 6.2
    *
    * Write a function to generate a `Double` between `0` and `1`, not including `1`. Note: You can
    * use `Int.MaxValue` to obtain the maximum positive integer value, and you can use `x.toDouble`
    * to convert an `x: Int` to a `Double`.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)

    (i / (Int.MaxValue.toDouble + 1.0), r)
  }

  /**
    * Exercise 6.3
    *
    * Write functions to generate an `(Int, Double)` pair, a `(Double, Int)` pair, and a `(Double,
    * Double, Double)` 3-tuple. You should be able to reuse the functions you’ve already written.
    */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)

    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)

    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  /**
    * Exercise 6.4
    *
    * Write a function to generate a list of random integers.
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(c: Int, acc: List[Int], r: RNG): (List[Int], RNG) = c match {
      case 0 => (acc, r)
      case _ => val (i, rn) = r.nextInt; loop(c - 1, i :: acc, rn)
    }

    loop(count, Nil, rng)
  }

  /**
    * Exercise 6.5
    *
    * Use map to reimplement `double` in a more elegant way. See exercise 6.2.
    */
  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1.0))

  /**
    * Exercise 6.6
    *
    * Write the implementation of `map2` based on the following signature. This function takes two
    * actions, `ra` and `rb`, and a function `f` for combining their results, and returns a new
    * action that combines them:
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)

    (f(a, b), rng2)
  }

  /**
    * Exercise 6.7
    *
    * Hard: If you can combine two `RNG` transitions, you should be able to combine a whole list of
    * them. Implement `sequence` for combining a `List` of transitions into a single transition. Use
    * it to reimplement the `ints` function you wrote before. For the latter, you can use the
    * standard library function `List.fill(n)(x)` to make a list with `x` repeated `n` times.
    */
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))

  /** Note the signature difference between this and the [[ints]] method. */
  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  /**
    * Exercise 6.8
    *
    * Implement `flatMap`, and then use it to implement `nonNegativeLessThan`.
    */
  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => { val (a, rng1) = ra(rng) ; f(a)(rng1) }

  /** Note main logic is taken from the book implementation. */
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {
      i => val mod = i % n ; if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  /**
    * Exercise 6.9
    *
    * Reimplement `map` and `map2` in terms of `flatMap`. The fact that this is possible is what
    * we’re referring to when we say that `flatMap` is more powerful than `map` and `map2`.
    *
    * ==Derivation==
    *
    * mapViaFlatMap(int)(_ + 1)
    *
    * becomes:
    * flatMap(int)(i => unit(i + 1))
    *
    * becomes:
    * r => { val (i, r2) = int(r) ; unit(i + 1)(r1) }
    */
  def mapViaFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  /**
    * ==Derivation==
    *
    * map2ViaFlatMap(int, double)((_, _))
    *
    * becomes:
    * flatMap(int)(i => mapViaFlatMap(double)(d => (i, d)))
    *
    * becomes:
    * flatMap(int)(i => flatMap(double)(d => unit((i, d))))
    *
    * becomes:
    * rng => { val (i, rng2) = int(rng); flatMap(double)(d => unit((i, d)))(rng2) }
    *
    * becomes:
    * val inner = rng => { val (d, rng2) = double(rng); unit((i, d))(rng2) }
    * rng => { val (i, rng2) = int(rng); inner(rng2) }
    */
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))
}

/**
  * Exercise 6.10
  *
  * Generalize the functions `unit`, `map`, `map2`, `flatMap`, and `sequence`. Add them as methods
  * on the `State` case class where possible. Otherwise you should put them in a `State` companion
  * object.
  */

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => { val (a, s2) = run(s); f(a).run(s2) })
}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](as: List[State[S, A]]): State[S, List[A]] =
    as.foldRight(unit[S, List[A]](Nil))(_.map2(_)(_ :: _))

  /** Uses for-comprehension for illustration. */
  def sequenceViaForComprehension[S, A](as: List[State[S, A]]): State[S, List[A]] =
    as.foldRight(unit[S, List[A]](Nil))((a, bs) => for {aa <- a; bbs <- bs} yield aa :: bbs)

  /**
    * Lifts a state transformation to the `State` context.
    *
    * Given a state transformation `f` of type `S => S`, this method returns a `State` instance
    * whose `run` method, when called, will return `(Unit, S)` where the result `S` is the result of
    * applying `f` to the input `S`.
    *
    * ==Derivation==
    * val update: Machine => Machine = _.copy(locked = false)
    * modify(update)
    *
    * becomes:
    * for { s <- get ; _ <- set(update(s)) } yield ()
    *
    * becomes:
    * get flatMap (s => set(update(s)) map (_ => ()))
    *
    * becomes:
    * State(s => (s, s)) flatMap (s => set(update(s)) map (_ => ()))
    *
    * becomes:
    * val stateUpdate: Machine => State[Machine, Unit] = s => set(update(s)) map (_ => ())
    * State(s => { val(ss, s2) = run(s) ; stateUpdate(ss).run(s2) })
    *
    * becomes:
    * State(s => { val (s1, s2) = run(s) ; State(_ => ((), update(s1))).run(s2) })
    *
    * becomes:
    * val m = Machine(true, 0, 0)
    * State(s => { val (s1, s2) = run(s); State(_ => ((), update(s1))).run(s2) }).run(m)
    *
    * becomes:
    * ((), Machine(false, 0, 0))
    *
    */
  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  /**
    * Returns a `State` instance whose `run` method returns the provided input.
    *
    * This is like an identity lifted to the `State` context.
    *
    * ==Derivation==
    * val s = get[Machine]
    *
    * becomes:
    * State[Machine, Machine](m => (m, m))
    *
    * when given a machine:
    * s.run(Machine(true, 0, 0))
    *
    * becomes:
    * (Machine(true, 0, 0), Machine(true, 0, 0))
    */
  def get[S]: State[S, S] = State(s => (s, s))

  /**
    * Returns a `State` instance whose `run` method returns the supplied state.
    *
    * When the `run` method is called on this `State`, its input is ignored and `S` provided
    * to this method is returned, along with a `Unit` result.
    *
    * ==Derivation==
    * val m = Machine(true, 0, 0)
    * val s = set[Machine](m)
    *
    * becomes:
    * State(_ => ((), m))
    *
    * when given a machine:
    * s.run(Machine(false, 666, 666))
    *
    * becomes:
    * ((), Machine(true, 0, 0))
    */
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

/**
  * Exercise 6.11
  *
  * Hard: To gain experience with the use of `State`, implement a finite state automaton that models
  * a simple candy dispenser. The machine has two types of input: you can insert a coin, or you can
  * turn the knob to dispense candy. It can be in one of two states: locked or unlocked. It also
  * tracks how many candies are left and how many coins it contains.
  *
  * The rules of the machine are as follows:
  *
  *   - Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
  *   - Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
  *   - Turning the knob on a locked machine or inserting a coin into an unlocked machine does
  *     nothing.
  *   - A machine that’s out of candy ignores all inputs.
  *
  * The method `simulateMachine` should operate the machine based on the list of inputs and return
  * the number of coins and candies left in the machine at the end. For example, if the input
  * `Machine` has 10 coins and 5 candies, and a total of 4 candies are successfully bought, the
  * output should be `(14, 1)`.
  */
object Candy {

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  // Case class representing a candy machine
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (update andThen modify))
      s <- get
    } yield (s.coins, s.candies)

  // State transitions
  val update: Input => Machine => Machine =
    input => machine => (input, machine) match {
      case (_, Machine(_, 0, _))        => machine // ignore all when no candies left
      case (Coin, Machine(false, _, _)) => machine // ignore coin when unlocked
      case (Turn, Machine(true, _, _))  => machine // ignore turn when locked
      case (Coin, Machine(true, c, m))  => Machine(locked = false, c, m + 1) // valid coin
      case (Turn, Machine(false, c, m)) => Machine(locked = true, c - 1, m) // valid turn
    }
}

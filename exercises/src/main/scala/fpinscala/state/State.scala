package fpinscala.state


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
    * Function composition on a state transition. Note that the state is unchanged; we apply `f` to the return value of
    * the state transition, and return the result, along with the new state computed by the transition.
    */
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
    * Exercise 6.1 - Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue
    * (inclusive). Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn’t have a
    * non-negative counterpart.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt

    (if (i < 0) -(i + 1) else i, r)
  }

  /**
    * Exercise 6.2 - Write a function to generate a Double between 0 and 1, not including 1. Note: You can use
    * Int.MaxValue to obtain the maximum positive integer value, and you can use x.toDouble to convert an x: Int to a
    * Double.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)

    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /**
    * Exercise 6.3 - Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double,
    * Double) 3-tuple. You should be able to reuse the functions you’ve already written.
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
    * Exercise 6.4 - Write a function to generate a list of random integers.
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
    * Exercise 6.5 - Use map to re-implement double in a more elegant way. See exercise 6.2.
    *
    * Note that `map` takes a `Rand[A]` and a function from `A` to `B` and yields a `Rand[B]`. Because `Rand[T]` is a
    * type alias for `RNG => (T, RNG)` this is the same as:
    *
    * def map(s: RNG => (A, RNG))(f: A => B): RNG => (B, RNG) = ...
    *
    * Note that our implementation of `map` is just function composition on a state transition. The new state is
    * computed by evaluating `s` with an input `RNG` instance. The application of `f` does not create a new state.
    *
    * This means that any function implemented in terms of `map` will automatically handle returning the _correct_ `RNG`
    * instance in its output, based on our implementation of `map`.
    */
  val doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1D))

  /**
    * Exercise 6.6 - Write the implementation of map2 based on the following signature. This function takes two actions,
    * `ra` and `rb`, and a function `f` for combining their results, and returns a new action that combines them.
    *
    * Note that because we cannot implement this function in terms of `map` we must explicitly handle the passing of the
    * correct `RNG` instance throughout the function.
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (aa, r1) = ra(rng)
    val (bb, r2) = rb(r1)

    (f(aa, bb), r2)
  }

  /**
    * Exercise 6.7 - Hard: If you can combine two `RNG` transitions, you should be able to combine a whole list of them.
    * Implement `sequence` for combining a List of transitions into a single transition. Use it to re-implement the
    * `ints` function you wrote before. For the latter, you can use the standard library function `List.fill(n)(x)` to
    * make a list with `x` repeated `n` times.
    *
    * Note that we have already implemented `map2` to combine two `RNG` transitions. That, along with iterating through
    * a list of `Rand[A]` instances suggests that combining `map2` with a `foldRight` is the correct implementation.
    *
    * Note that the `z` parameter to `foldRight` is `unit(Nil)` which is a data constructor that takes a constant and
    * lifts it to an appropriate `Rand` instance.
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((f, acc) => map2(f, acc)(_ :: _))

  /**
    * Note that the signature of this implementation is different from `ints` above.
    */
  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  /**
    * Exercise 6.8 - Implement `flatMap`, and then use it to implement `nonNegativeLessThan`.
    *
    * Note that we perform the state transition in our function `f` and apply the function `g` to the returned result.
    * Applying `g(a)` to the new state is what is responsible for "flattening" the return value; we manage the passing
    * of states inside the function.
    */

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)

    g(a)(rng1)
  }

  /**
    * Remember that `flatMap` evaluates its `g` function with respect to the new state obtained by first evaluating its
    * `f` function - in the case of `nonNegativeLessThan` we are able to recursively call the same function while
    * automatically passing the newest states.
    */
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) {
    i => val mod = i % n ; if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  /**
    * Exercise 6.9 - Re-implement `map` and `map2` in terms of `flatMap`. The fact that this is possible is what we’re
    * referring to when we say that `flatMap` is more powerful than `map` and `map2`.
    */

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))
}

/**
  * Exercise 6.10 - Generalize the functions `unit`, `map`, `map2`, `flatMap`, and `sequence`. Add them as methods on
  * the `State` case class where possible. Otherwise you should put them in a `State` companion object.
  */
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = for {
      a <- this
      b <- sb
    } yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => { val (a, s2) = run(s); f(a).run(s2) })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit(Nil): State[S, List[A]])((s, acc) => { for { h <- s ; t <- acc } yield h :: t })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  type Rand[A] = State[RNG, A]

  /**
    * Exercise 6.11 - Hard: To gain experience with the use of `State`, implement a finite state automaton that models a
    * simple candy dispenser. The machine has two types of input: you can insert a coin, or you can turn the knob to
    * dispense candy. It can be in one of two states: locked or unlocked. It also tracks how many candies are left and
    * how many coins it contains.
    *
    * The rules of the machine are as follows:
    *
    *   - Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    *   - Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
    *   - Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
    *   - A machine that’s out of candy ignores all inputs.
    *
    * The method `simulateMachine` should operate the machine based on the list of inputs and return the number of coins
    * and candies left in the machine at the end. For example, if the input `Machine` has 10 coins and 5 candies, and a
    * total of 4 candies are successfully bought, the output should be `(14, 1)`.
    *
    * Note the approach here is to map the list of inputs to transitions of the candy machine's state using the `modify`
    * method. This method expects a function `Machine => Machine` which we create based on the rules outlined above.
    * This yields a `List` of state transitions, which can be collapsed by using the `sequence` function we defined
    * above.
    *
    * This attempt makes a few bad choices, though:
    *
    *   - The machine transitions are encoded as anonymous functions; this is a little clunky
    *   - The state of the machine (in terms of candies and coins) is produced for each input; only the last is required
    *   - The final count of candies and coins is obtained in an unsafe way; throws `NoSuchElementException` if empty
    *
    * A better solution is provided in the second example. The major differences include:
    *
    *   - Modeling the candy machine updates in the `Candy` object and using pattern matching for better clarity
    *   - Cleaner implementation in `sequence`, using function composition of `update` and `modify`
    *   - Obtaining the final machine state once, after processing all inputs, in a safe way
    */

//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
//    val machineT: (Machine => Machine) => State[Machine, (Int, Int)] = f => for {
//      _ <- modify(f)
//      s <- get
//    } yield (s.coins, s.candies)
//
//    val coinT = machineT(m => m.candies match { case 0 => m ; case _ => m.copy(locked = false, coins = m.coins + 1) })
//    val turnT = machineT(m => m.candies match { case 0 => m ; case n => m.copy(locked = true, candies = if (m.locked) n else n - 1) })
//
//    // Transform inputs into a state machine
//    val machine = sequence(inputs map { case Coin => coinT ; case Turn => turnT })
//
//    // We're only interested in the final result
//    machine map (_.last)
//  }

  object Candy {
    val update: Input => Machine => Machine = i => m => (i, m) match {
      case (_, Machine(_, 0, _))               => m                                        // ran out of candies
      case (Coin, Machine(false, _, _))        => m                                        // adding coin when open
      case (Turn, Machine(true, _, _))         => m                                        // turning knob when locked
      case (Coin, Machine(true, candy, coin))  => Machine(locked = false, candy, coin + 1) // adding coin when locked
      case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)  // turning knob when open
    }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- sequence(inputs map (update andThen modify))
      s <- get
    } yield (s.coins, s.candies)
  }
}

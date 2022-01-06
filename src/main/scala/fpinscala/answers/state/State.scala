package fpinscala.answers.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r)

  // We generate an integer >= 0 and divide it by one higher than the
  // maximum. This is just one possible solution.
  def double(rng: RNG): (Double, RNG) =
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match
      case (i,rng2) => (i%2==0,rng2)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  // A simple recursive solution
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count <= 0 then
      (List(), rng)
    else
      val (x, r1)  = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)

  // A tail-recursive solution
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if count <= 0 then
        (xs, r)
      else
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
    go(count, rng, List())

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  val _double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // This implementation of map2 passes the initial RNG to the first argument
  // and the resulting RNG to the second argument. It's not necessarily wrong
  // to do this the other way around, since the results are random anyway.
  // We could even pass the initial RNG to both `f` and `g`, but that might
  // have unexpected results. E.g. if both arguments are `RNG.int` then we would
  // always get two of the same `Int` in the result. When implementing functions
  // like this, it's important to consider how we would test them for
  // correctness.
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng0 =>
      val (a, rng1) = ra(rng0)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // In `sequence`, the base case of the fold is a `unit` action that returns
  // the empty list. At each step in the fold, we accumulate in `acc`
  // and `r` is the current element in the list.
  // `map2(r, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
  // We map over that to prepend (cons) the element onto the accumulated list.
  //
  // We are using `foldRight`. If we used `foldLeft` then the values in the
  // resulting list would appear in reverse order. It would be arguably better
  // to use `foldLeft` followed by `reverse`. What do you think?
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))

  // It's interesting that we never actually need to talk about the `RNG` value
  // in `sequence`. This is a strong hint that we could make this function
  // polymorphic in that type.

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng0 =>
      val (a, rng1) = r(rng0)
      f(a)(rng1)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- underlying
        b <- sb
      yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] = 
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    actions.foldRight(unit[S, List[A]](Nil))((f, acc) => f.map2(acc)(_ :: _))

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    yield ()

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = 
    for
      _ <- State.traverse(inputs)(i => State.modify(update(i)))
      s <- State.get
    yield (s.coins, s.candies)

  val update = (i: Input) => (s: Machine) =>
    (i, s) match
      case (_, Machine(_, 0, _)) => s
      case (Input.Coin, Machine(false, _, _)) => s
      case (Input.Turn, Machine(true, _, _)) => s
      case (Input.Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Input.Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)


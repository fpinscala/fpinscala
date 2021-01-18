package fpinscala.state

import fpinscala.state.RNG.{Simple, double, nonNegativeInt}

// ch 6.4 a better api for state actions
// build on top of RNG
object Rand {

  // type alias for RNG state action, or RNG state transition - this is a lambda, or a function
  type Rand[+A] = RNG => (A, RNG)

  def main(args: Array[String]): Unit = {
    println("Rand.main()")

    println(int) // fpinscala.state.Rand$$$Lambda$8/431687835@4563e9ab
    println("int:" + int(Simple(42))) // (16159453,Simple(1059025964525))

    // this is the same as defining the lambda directly
    val intRngLambda: RNG => (Int, RNG) = rng => rng.nextInt
    println("intRngLambda:" + intRngLambda(Simple(42))) // SAME

    // this is the same as calling the class directly
    val rng = Simple(42)
    println("rng:" + rng.nextInt) // SAME

    println("unit:" + unit(10)(Simple(42))) // (10,Simple(42))
    println("unit:" + unit("whatever")(Simple(42))) // (whatever,Simple(42))

    println("map:" + map(int)(_ + 1)(Simple(42))) // (16159454,Simple(1059025964525)) // note +1 is working
    println("_double:" + _double(Simple(42))) // (0.007524831686168909,Simple(1059025964525))

    println("map2:" + map2(int, int)(_ + _)(Simple(42))) // (-1265320244,Simple(197491923327988))
    println("both:" + both(int, int)(Simple(42))) // ((16159453,-1281479697),Simple(197491923327988))
    println("randIntDouble:" + randIntDouble(Simple(42))) // ((16159453,0.5967354848980904),Simple(197491923327988))

    val intRands2 = List(int, int)
    println("sequence2:" + sequence(intRands2)(Simple(42))) // (List(16159453, -1281479697),Simple(197491923327988))
    val intRands3 = List(int, int, int)
    println("sequence3:" + sequence(intRands3)(Simple(42))) // (List(16159453, -1281479697, -340305902),Simple(259172689157871))
    println("_ints:" + _ints(3)(Simple(42))) // SAME as sequence3 above

  }

  // not sure whether this is still used ???
  def int: Rand[Int] = _.nextInt

  // simplest state action: same state
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def _double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // This implementation of map2 passes the initial RNG to the first argument
  // and the resulting RNG to the second argument. It's not necessarily wrong
  // to do this the other way around, since the results are random anyway.
  // We could even pass the initial RNG to both `f` and `g`, but that might
  // have unexpected results. E.g. if both arguments are `RNG.int` then we would
  // always get two of the same `Int` in the result. When implementing functions
  // like this, it's important to consider how we would test them for
  // correctness.
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // In `sequence`, the base case of the fold is a `unit` action that returns
  // the empty list. At each step in the fold, we accumulate in `acc`
  // and `f` is the current element in the list.
  // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
  // We map over that to prepend (cons) the element onto the accumulated list.
  //
  // We are using `foldRight`. If we used `foldLeft` then the values in the
  // resulting list would appear in reverse order. It would be arguably better
  // to use `foldLeft` followed by `reverse`. What do you think?
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight( unit(List[A]()) )( (f, acc) => map2(f, acc)(_ :: _) )

  // It's interesting that we never actually need to talk about the `RNG` value
  // in `sequence`. This is a strong hint that we could make this function
  // polymorphic in that type.

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1) // We pass the new state along to g(a): Rand[B]
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)( a => map(rb)(b => f(a, b)) )

}

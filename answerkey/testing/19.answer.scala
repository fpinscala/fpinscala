def genStringFn[A](g: Gen[A]): Gen[String => A] =
  State[RNG, String => A] { rng =>
    val (seed, rng2) = rng.nextInt // we still use `rng` to produce a seed, so we get a new function each time
    val f = (s: String) => g.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
    (f, rng2) 
  }

opaque type Cogen[-A] = (A, RNG) => RNG

object Cogen:
  def fn[A, B](in: Cogen[A], out: Gen[B]): Gen[A => B] =
    State[RNG, A => B] { rng =>
      val (seed, rng2) = rng.nextInt
      val f = (a: A) => out.run(in(a, rng2))._1
      (f, rng2)
    }

  def cogenInt: Cogen[Int] = (i, rng) =>
    val (seed, rng2) = rng.nextInt
    RNG.Simple(seed.toLong ^ i.toLong)

  // We can now write properties that depend on arbitrary functions
  def takeWhilePropInt =
    forAll(Gen.int.list ** fn(cogenInt, Gen.boolean).unsized)((ys, f) => ys.takeWhile(f).forall(f))

  // And we can further generalize those properties to be parameterized by types which are not relevant
  def takeWhileProp[A](ga: Gen[A], ca: Cogen[A]) =
    forAll(ga.list ** fn(ca, Gen.boolean).unsized)((ys, f) => ys.takeWhile(f).forall(f))
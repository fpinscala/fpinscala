package fpinscala.answers.testing.exhaustive

import annotation.targetName
import scala.util.control.NonFatal

/*
This source file contains the answers to the last two exercises in the section
"Test Case Minimization" of chapter 8 on property-based testing.

The Gen data type in this file incorporates exhaustive checking of finite domains.
*/

import fpinscala.answers.state.*
import fpinscala.answers.parallelism.*
import fpinscala.answers.parallelism.Par
import Gen.*
import Prop.*
import Status.*
import Result.*
import java.util.concurrent.{Executors, ExecutorService}

opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x

  opaque type FailedCase = String
  object FailedCase:
    extension (f: FailedCase) def string: String = f
    def fromString(s: String): FailedCase = s

  enum Status:
    case Proven, Unfalsified

  enum Result:
    case Falsified(failure: FailedCase)
    case Passed(status: Status, testCases: TestCases)

  extension (self: Prop)
    def &&(that: Prop): Prop =
      (max, n, rng) => self.tag("and-left")(max, n, rng) match
        case Passed(a, n) => that.tag("and-right")(max, n, rng) match
          case Passed(s, m) => Passed(s, TestCases.fromInt(n.toInt + m.toInt))
          case x => x
        case x => x

    def ||(that: Prop): Prop =
      (max, n, rng) => self.tag("or-left")(max, n, rng) match
        case Falsified(msg) => that.tag("or-right").tag(msg.string)(max, n, rng)
        case x => x

    def tag(msg: String): Prop =
      (max, n, rng) => self(max, n, rng) match
        case Falsified(e) => Falsified(FailedCase.fromString(s"$msg($e)"))
        case x => x

    def run(
          maxSize: MaxSize = 100,
          testCases: TestCases = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
      self(maxSize, testCases, rng) match
        case Falsified(msg) => println(s"! Failed:\n $msg")
        case Passed(Unfalsified, n) =>
          println(s"+ OK, property unfalsified, ran $n tests.")
        case Passed(Proven, n) =>
          println(s"+ OK, property proven, ran $n tests.")
        // case Passed(Exhausted, n) =>
        //   println(s"+ OK, property unfalsified up to max size, ran $n tests.")

    def check(
              maxSize: MaxSize = 100,
              testCases: TestCases = 100,
              rng: RNG = RNG.Simple(System.currentTimeMillis)
            ): Result =
      self(maxSize, testCases, rng)

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    (max, n, rng) => {
      def go(i: Int, j: Int, l: LazyList[Option[A]], onEnd: Int => Result): Result =
        if i == j then Passed(Unfalsified, i)
        else l match
          case Some(h) #:: t =>
            try
              if f(h) then go(i+1, j, t, onEnd)
              else Falsified(h.toString)
            catch
              case NonFatal(e) => Falsified(buildMsg(h, e))
          case None #:: _ => Passed(Unfalsified, i)
          case _ => onEnd(i)
      val numFromExhaustiveList = TestCases.fromInt(n.toInt / 3)
      go(0, numFromExhaustiveList, a.exhaustive, i => Passed(Proven, i)) match
        case Passed(Unfalsified, _) =>
          val rands = randomLazyList(a)(rng).map(Some(_))
          go(numFromExhaustiveList, n, rands, i => Passed(Unfalsified, i))
        case s => s // If proven or failed, stop immediately
    }

  def buildMsg[A](s: A, e: Throwable): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n,rng)

  /* We pattern match on the `SGen`, and delegate to our `Gen` version of `forAll`
   * if `g` is unsized; otherwise, we call the sized version of `forAll` (below).
   */
  @targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = g match
    case SGen.Unsized(g2) => forAll(g2)(f)
    case SGen.Sized(gs) => forAll(gs)(f)

  /* The sized case of `forAll` is as before, though we convert from `Proven` to
   * `Unfalsified`. A sized generator can never be proven, since there are always
   * larger-sized tests that were not run which may have failed.
   */
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      val casesPerSize = TestCases.fromInt(n.toInt / max + 1)
      val props: List[Prop] =
        LazyList.from(0).take(max.toInt + 1).map(i => forAll(g(i))(f)).toList
      val p: Prop = props.map[Prop](p => (max, n, rng) => p(max, casesPerSize, rng)).
            reduceLeft(_ && _)
      p(max, n, rng) match
        case Passed(Proven, n) => Passed(Unfalsified, n)
        case x => x

  val executor: ExecutorService = Executors.newCachedThreadPool

  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(pi =>
    pi.map(_ + 1).run(executor).get == Par.unit(2).run(executor).get)

  def check(p: => Boolean): Prop =
    (_, _, _) => Passed(Proven, 1)

  val p2 = check {
    val p = Par.unit(1).map(_ + 1)
    val p2 = Par.unit(2)
    p.run(executor).get == p2.run(executor).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    p.map2(p2)(_ == _)

  val p3 = check {
    equal(
      Par.unit(1).map(_ + 1),
      Par.unit(2)
    ).run(executor).get
  }

  val executors: Gen[ExecutorService] = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a, b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(executors ** g)((s, a) => f(a).run(s).get)

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(executors ** g)((s, a) => f(a).run(s).get)

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(executors ** g) { case s ** a => f(a).run(s).get }

  val gpy: Gen[Par[Int]] = Gen.choose(0, 10).map(Par.unit(_))
  val p4 = forAllPar(gpy)(py => equal(py.map(y => y), py))

  lazy val gpy2: Gen[Par[Int]] = choose(-100, 100).listOfN(choose(0, 20)).map(ys =>
    ys.foldLeft(Par.unit(0))((p, y) =>
      Par.fork(p.map2(Par.unit(y))(_ + _))))

  val forkProp = Prop.forAllPar(gpy2)(y => equal(Par.fork(y), y))

/*
The `Gen` type now has a random generator as well as an exhaustive lazy list.
Infinite domains will simply generate infinite lazy lists of None.
A finite domain is exhausted when the lazy list reaches empty.
*/
case class Gen[+A](sample: State[RNG, A], exhaustive: LazyList[Option[A]]):
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f), exhaustive.map(_.map(f)))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f),
        map2LazyList(exhaustive, g.exhaustive)(map2Option(_, _)(f)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample),
        exhaustive.flatMap {
          case None => unbounded
          case Some(a) => f(a).exhaustive
        })

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => this.listOfN(n))

  def list: SGen[List[A]] = SGen.Sized(n => listOfN(n))
  def nonEmptyList: SGen[List[A]] = SGen.Sized(n => listOfN(n.max(1)))

  def unsized: SGen[A] = SGen.Unsized(this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    map2(g)((_, _))

object Gen:
  type Domain[+A] = LazyList[Option[A]]

  def bounded[A](a: LazyList[A]): Domain[A] = a.map(Some(_))
  def unbounded: Domain[Nothing] = LazyList(None)

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a), bounded(LazyList(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean), bounded(LazyList(true,false)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)),
        bounded(LazyList.from(start).take(stopExclusive-start)))

  /* This implementation is rather tricky, but almost impossible to get wrong
   * if you follow the types. It relies on several helper functions (see below).
   */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)),
        cartesian(LazyList.continually(g.exhaustive).take(n)).
        map(l => sequenceOption(l.toList)))

  /* `cartesian` generates all possible combinations of a `LazyList[LazyList[A]]`. For instance:
   *
   *    cartesian(LazyList(LazyList(1,2), LazyList(3), LazyList(4,5))) ==
   *    LazyList(LazyList(1,3,4), LazyList(1,3,5), LazyList(2,3,4), LazyList(2,3,5))
  */
  def cartesian[A](s: LazyList[LazyList[A]]): LazyList[LazyList[A]] =
    s.foldRight(LazyList(LazyList[A]()))((hs,ts) => map2LazyList(hs,ts)(LazyList.cons(_,_)))

  /* `map2Option` and `map2LazyList`. Notice the duplication! */
  def map2Option[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for
      a <- oa
      b <- ob
    yield f(a, b)

  /* This is not the same as `zipWith`, a function we've implemented before.
   * We are generating all (A,B) combinations and using each to produce a `C`.
   * This implementation desugars to sa.flatMap(a => sb.map(b => f(a,b))).
   */
  def map2LazyList[A, B, C](sa: LazyList[A], sb: => LazyList[B])(f: (A, =>B) => C): LazyList[C] =
    for
      a <- sa
      b <- sb
    yield f(a, b)

  /* This is a function we've implemented before. Unfortunately, it does not
   * exist in the standard library. This implementation is uses a foldLeft,
   * followed by a reverse, which is equivalent to a foldRight, but does not
   * use any stack space.
   */
  def sequenceOption[A](o: List[Option[A]]): Option[List[A]] =
    o.foldLeft[Option[List[A]]](Some(List()))(
      (t,h) => map2Option(h,t)(_ :: _)).map(_.reverse)

  /* Notice we are using the `unbounded` definition here, which is just
   * `LazyList(None)` in our current representation of `exhaustive`.
   */
  def double: Gen[Double] = Gen(State(RNG.double), unbounded)
  def int: Gen[Int] = Gen(State(RNG.int), unbounded)

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(State(RNG.double).map(d => i + d*(j-i)), unbounded)

  /* Basic idea is add 1 to the result of `choose` if it is of the wrong
   * parity, but we require some special handling to deal with the maximum
   * integer in the range.
   */
  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if stopExclusive%2 == 0 then stopExclusive - 1 else stopExclusive).
    map (n => if n%2 != 0 then n+1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if stopExclusive%2 != 0 then stopExclusive - 1 else stopExclusive).
    map (n => if n%2 == 0 then n+1 else n)

  def sameParity(from: Int, to: Int): Gen[(Int,Int)] = for
    i <- choose(from,to)
    j <- if (i%2 == 0) even(from,to) else odd(from,to)
  yield (i,j)

  def listOfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(unit(List[A]()))((a,b) => a.map2(b)(_ :: _))

  /* The simplest possible implementation. This will put all elements of one
   * `Gen` before the other in the exhaustive traversal. It might be nice to
   * interleave the two lazy lists, so we get a more representative sample if we
   * don't get to examine the entire exhaustive lazy list.
   */
  def union_1[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if b then g1 else g2)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen(
      State(RNG.boolean).flatMap(b => if b then g1.sample else g2.sample),
      interleave(g1.exhaustive, g2.exhaustive)
    )

  def interleave[A](s1: LazyList[A], s2: LazyList[A]): LazyList[A] =
    s1.map(Some(_)).zipAll(s2.map(Some(_)), None, None).flatMap((a,a2) => LazyList((a.toList ++ a2.toList)*))

  /* The random case is simple - we generate a double and use this to choose between
   * the two random samplers. The exhaustive case is trickier if we want to try
   * to produce a lazy list that does a weighted interleave of the two exhaustive lazy list.
   */
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    /* Some random booleans to use for selecting between g1 and g2 in the exhaustive case.
     * Making up a seed locally is fine here, since we just want a deterministic schedule
     * with the right distribution. */
    def bools: LazyList[Boolean] =
      randomLazyList(double.map(_ < g1Threshold))(RNG.Simple(302837L))

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample),
        interleave(bools, g1._1.exhaustive, g2._1.exhaustive))

  /* Produce an infinite random lazy list from a `Gen` and a starting `RNG`. */
  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  /* Interleave the two lazy list, using `b` to control which lazy list to pull from at each step.
   * A value of `true` attempts to pull from `s1`; `false` attempts to pull from `s1`.
   * When either lazy list is exhausted, insert all remaining elements from the other lazy list.
   */
  def interleave[A](b: LazyList[Boolean], s1: LazyList[A], s2: LazyList[A]): LazyList[A] =
    b.headOption.map { hd =>
      if hd then s1 match
        case h #:: t => LazyList.cons(h, interleave(b drop 1, t, s2))
        case _ => s2
      else s2 match
        case h #:: t => LazyList.cons(h, interleave(b drop 1, s1, t))
        case _ => s1
    }.getOrElse(LazyList.empty)

  /* Not the most efficient implementation, but it's simple.
   * This generates ASCII strings.
   */
  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

  def string: SGen[String] = SGen.Sized(stringN)

  val smallInt = Gen.choose(-10,10)
  val maxProp = forAll(smallInt.list) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  val maxProp1 = forAll(smallInt.nonEmptyList) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  val sortedProp = Prop.forAll(smallInt.list) { l =>
    val ls = l.sorted
    val ordered = l.isEmpty || ls.zip(ls.tail).forall { (a, b) => a <= b }
    ordered && l.forall(ls.contains) && ls.forall(l.contains)
  }

  object ** :
    def unapply[A,B](p: (A,B)) = Some(p)

  /* A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
   * computation for each element of the input list summed to produce the final
   * result. This is not the most compelling example, but it provides at least some
   * variation in structure to use for testing.
   */
  lazy val pint2: Gen[Par[Int]] = choose(-100,100).listOfN(choose(0,20)).map(l =>
    l.foldLeft(Par.unit(0))((p, i) =>
      Par.fork { p.map2(Par.unit(i))(_ + _) }))

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g.map(i => (s => i))

  def genStringFn[A](g: Gen[A]): Gen[String => A] =
    val sample = State[RNG, String => A] { rng =>
      val (seed, rng2) = rng.nextInt // we still use `rng` to produce a seed, so we get a new function each time
      val f = (s: String) => g.sample.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
      (f, rng2)
    }
    Gen(sample, unbounded)

end Gen

enum SGen[+A]:
  case Sized(forSize: Int => Gen[A])
  case Unsized(get: Gen[A])

  def map[B](f: A => B): SGen[B] = this match
    case Sized(g) => Sized(g.andThen(_.map(f)))
    case Unsized(g) => Unsized(g.map(f))
  def flatMap[B](f: A => Gen[B]): SGen[B] = this match
    case Sized(g) => Sized(g.andThen(_.flatMap(f)))
    case Unsized(g) => Unsized(g.flatMap(f))
  def **[B](s2: SGen[B]): SGen[(A,B)] = (this,s2) match
    case (Sized(g), Sized(g2)) => Sized(n => g(n) ** g2(n))
    case (Unsized(g), Unsized(g2)) => Unsized(g ** g2)
    case (Sized(g), Unsized(g2)) => Sized(n => g(n) ** g2)
    case (Unsized(g), Sized(g2)) => Sized(n => g ** g2(n))

opaque type Cogen[-A] = (A, RNG) => RNG

object Cogen:

  def fn[A, B](in: Cogen[A], out: Gen[B]): Gen[A => B] =
    val sample = State[RNG, A => B] { rng =>
      val (seed, rng2) = rng.nextInt
      val f = (a: A) => out.sample.run(in(a, rng2))._1
      (f, rng2)
    }
    Gen(sample, unbounded)

  def cogenInt: Cogen[Int] = (i, rng) =>
    val (seed, rng2) = rng.nextInt
    RNG.Simple(seed.toLong ^ i.toLong)

  // We can now write properties that depend on arbitrary functions
  def takeWhilePropInt =
    forAll(Gen.int.list ** fn(cogenInt, Gen.boolean).unsized)((ys, f) => ys.takeWhile(f).forall(f))

  // And we can further generalize those properties to be parameterized by types which are not relevant
  def takeWhileProp[A](ga: Gen[A], ca: Cogen[A]) =
    forAll(ga.list ** fn(ca, Gen.boolean).unsized)((ys, f) => ys.takeWhile(f).forall(f))
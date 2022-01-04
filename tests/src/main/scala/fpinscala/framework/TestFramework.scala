package fpinscala.framework

import scala.annotation.targetName

/*
 A simple test framework implementation based on `fpinscala.testing.exhaustive.Props`
 */

import fpinscala.framework.Gen.*
import fpinscala.framework.Prop.*
import fpinscala.framework.Prop.Result.*
import fpinscala.framework.Prop.Status.*

import java.util.concurrent.{ExecutorService, Executors}

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
      (max, n, rng) =>
        self.tag("and-left")(max, n, rng) match
          case Passed(a, n) =>
            that.tag("and-right")(max, n, rng) match
              case Passed(s, m) => Passed(s, TestCases.fromInt(n.toInt + m.toInt))
              case x            => x
          case x => x

    def ||(that: Prop): Prop =
      (max, n, rng) =>
        self.tag("or-left")(max, n, rng) match
          case Falsified(msg) => that.tag("or-right").tag(msg.string)(max, n, rng)
          case x              => x

    def tag(msg: String): Prop =
      (max, n, rng) =>
        self(max, n, rng) match
          case Falsified(e) => Falsified(FailedCase.fromString(s"$msg($e)"))
          case x            => x

    def run(maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
      self(maxSize, testCases, rng) match
        case Falsified(msg) => println(s"! Failed:\n $msg")
        case Passed(Unfalsified, n) =>
          println(s"+ OK, property unfalsified, ran $n tests.")
        case Passed(Proven, n) =>
          println(s"+ OK, property proven, ran $n tests.")

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    (max, n, rng) => {
      def go(i: Int, j: Int, l: LazyList[Option[A]], onEnd: Int => Result): Result =
        if i == j then Passed(Unfalsified, i)
        else
          l match
            case Some(h) #:: t =>
              try
                if f(h) then go(i + 1, j, t, onEnd)
                else Falsified(h.toString)
              catch case e: Exception => Falsified(buildMsg(h, e))
            case None #:: _ => Passed(Unfalsified, i)
            case _          => onEnd(i)
      val numFromExhaustiveList = TestCases.fromInt(n.toInt / 3)
      go(0, numFromExhaustiveList, a.exhaustive, i => Passed(Proven, i)) match
        case Passed(Unfalsified, _) =>
          val rands = randomLazyList(a)(rng).map(Some(_))
          go(numFromExhaustiveList, n, rands, i => Passed(Unfalsified, i))
        case s => s // If proven or failed, stop immediately
    }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)

/*
The `Gen` type now has a random generator as well as an exhaustive lazy list.
Infinite domains will simply generate infinite lazy lists of None.
A finite domain is exhausted when the lazy list reaches empty.
 */
case class Gen[+A](sample: State[RNG, A], exhaustive: LazyList[Option[A]]):
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f), exhaustive.map(_.map(f)))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f), map2LazyList(exhaustive, g.exhaustive)(map2Option(_, _)(f)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(
      sample.flatMap(a => f(a).sample),
      exhaustive.flatMap {
        case None    => unbounded
        case Some(a) => f(a).exhaustive
      }
    )

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => this.listOfN(n))

  def **[B](g: Gen[B]): Gen[(A, B)] =
    map2(g)((_, _))

object Gen:
  type Domain[+A] = LazyList[Option[A]]

  def bounded[A](a: LazyList[A]): Domain[A] = a.map(Some(_))
  def unbounded: Domain[Nothing] = LazyList(None)

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a), bounded(LazyList(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean), bounded(LazyList(true, false)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(
      State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)),
      bounded(LazyList.from(start).take(stopExclusive - start))
    )

  /* This implementation is rather tricky, but almost impossible to get wrong
   * if you follow the types. It relies on several helper functions (see below).
   */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(
      State.sequence(List.fill(n)(g.sample)),
      cartesian(LazyList.continually(g.exhaustive).take(n)).map(l => sequenceOption(l.toList))
    )

  /* `cartesian` generates all possible combinations of a `LazyList[LazyList[A]]`. For instance:
   *
   *    cartesian(LazyList(LazyList(1,2), LazyList(3), LazyList(4,5))) ==
   *    LazyList(LazyList(1,3,4), LazyList(1,3,5), LazyList(2,3,4), LazyList(2,3,5))
   */
  def cartesian[A](s: LazyList[LazyList[A]]): LazyList[LazyList[A]] =
    s.foldRight(LazyList(LazyList[A]()))((hs, ts) => map2LazyList(hs, ts)(LazyList.cons(_, _)))

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
  def map2LazyList[A, B, C](sa: LazyList[A], sb: => LazyList[B])(f: (A, => B) => C): LazyList[C] =
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
    o.foldLeft[Option[List[A]]](Some(List()))((t, h) => map2Option(h, t)(_ :: _)).map(_.reverse)

  /* Notice we are using the `unbounded` definition here, which is just
   * `LazyList(None)` in our current representation of `exhaustive`.
   */
  def double: Gen[Double] = Gen(State(RNG.double), unbounded)
  def int: Gen[Int] = Gen(State(RNG.int), unbounded)

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(State(RNG.double).map(d => i + d * (j - i)), unbounded)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen(
      State(RNG.boolean).flatMap(b => if b then g1.sample else g2.sample),
      interleave(g1.exhaustive, g2.exhaustive)
    )

  def interleave[A](s1: LazyList[A], s2: LazyList[A]): LazyList[A] =
    s1.map(Some(_)).zipAll(s2.map(Some(_)), None, None).flatMap((a, a2) => LazyList((a.toList ++ a2.toList)*))

  /* The random case is simple - we generate a double and use this to choose between
   * the two random samplers. The exhaustive case is trickier if we want to try
   * to produce a lazy list that does a weighted interleave of the two exhaustive lazy list.
   */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    /* Some random booleans to use for selecting between g1 and g2 in the exhaustive case.
     * Making up a seed locally is fine here, since we just want a deterministic schedule
     * with the right distribution. */
    def bools: LazyList[Boolean] =
      randomLazyList(double.map(_ < g1Threshold))(RNG.Simple(302837L))

    Gen(
      State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample),
      interleave(bools, g1._1.exhaustive, g2._1.exhaustive)
    )

  /* Produce an infinite random lazy list from a `Gen` and a starting `RNG`. */
  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  /* Interleave the two lazy list, using `b` to control which lazy list to pull from at each step.
   * A value of `true` attempts to pull from `s1`; `false` attempts to pull from `s1`.
   * When either lazy list is exhausted, insert all remaining elements from the other lazy list.
   */
  def interleave[A](b: LazyList[Boolean], s1: LazyList[A], s2: LazyList[A]): LazyList[A] =
    b.headOption
      .map { hd =>
        if hd then
          s1 match
            case h #:: t => LazyList.cons(h, interleave(b drop 1, t, s2))
            case _       => s2
        else
          s2 match
            case h #:: t => LazyList.cons(h, interleave(b drop 1, s1, t))
            case _       => s1
      }
      .getOrElse(LazyList.empty)

  /* Not the most efficient implementation, but it's simple.
   * This generates ASCII strings.
   */
  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  object ** :
    def unapply[A, B](p: (A, B)) = Some(p)

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

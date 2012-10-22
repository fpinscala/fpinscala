def unit[A](a: => A): Gen[A] = 
  Gen(State.unit(a), bounded(Stream(a)))

def boolean: Gen[Boolean] = 
  Gen(State(RNG.boolean), bounded(Stream(true,false)))

def choose(start: Int, stopExclusive: Int): Gen[Int] = 
  Gen(State(RNG.positiveInt).map(n => start + n % (stopExclusive-start)),
      bounded(Stream.from(start).take(stopExclusive-start)))

/* This implementation is rather tricky, but almost impossible to get wrong
 * if you follow the types. It relies on several helper functions (see below). 
 */
def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = 
  Gen(State.sequence(List.fill(n)(g.sample)),
      cartesian(Stream.constant(g.exhaustive).take(n)).
      map(l => sequenceOption(l.toList)))

/* `cartesian` generates all possible combinations of a `Stream[Stream[A]]`. For instance: 
 *
 *    cartesian(Stream(Stream(1,2), Stream(3), Stream(4,5))) ==
 *    Stream(Stream(1,3,4), Stream(1,3,5), Stream(2,3,4), Stream(2,3,5))
*/
def cartesian[A](s: Stream[Stream[A]]): Stream[Stream[A]] = 
  s.foldRight(Stream(Stream[A]()))((hs,ts) => map2Stream(hs,ts)(Stream.cons(_,_))) 

/* `map2Option` and `map2Stream`. Notice the duplication! */
def map2Option[A,B,C](oa: Option[A], ob: Option[B])(f: (A,B) => C): Option[C] = 
  for { a <- oa; b <- ob } yield f(a,b)

/* This is not the same as `zipWith`, a function we've implemented before. 
 * We are generating all (A,B) combinations and using each to produce a `C`. 
 * This implementation desugars to sa.flatMap(a => sb.map(b => f(a,b))).
 */
def map2Stream[A,B,C](sa: Stream[A], sb: => Stream[B])(f: (A,=>B) => C): Stream[C] = 
  for { a <- sa; b <- sb } yield f(a,b)

/* This is a function we've implemented before. Unfortunately, it does not 
 * exist in the standard library. This implementation is uses a foldLeft,
 * followed by a reverse, which is equivalent to a foldRight, but does not
 * use any stack space.
 */
def sequenceOption[A](o: List[Option[A]]): Option[List[A]] = 
  o.foldLeft[Option[List[A]]](Some(List()))(
    (t,h) => map2Option(h,t)(_ :: _)).map(_.reverse)

/* Notice we are using the `unbounded` definition here, which is just 
 * `Stream(None)` in our current representation of `exhaustive`. 
 */
def uniform: Gen[Double] = 
  Gen(State(RNG.double), unbounded)

def choose(i: Double, j: Double): Gen[Double] = 
  Gen(State(RNG.double).map(d => i + d*(j-i)), unbounded)
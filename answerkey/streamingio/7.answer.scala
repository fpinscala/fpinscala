/*
 * Exercise 7: Can you think of a generic combinator that would
 * allow for the definition of `mean` in terms of `sum` and
 * `count`?
 *
 * Yes, it is `zip`, which feeds the same input to two processes.
 * The implementation is a bit tricky, as we have to make sure
 * that input gets fed to both `p1` and `p2`.
 */
def zip[A,B,C](p1: Process[A,B], p2: Process[A,C]): Process[A,(B,C)] =
  (p1, p2) match {
    case (Halt(), _) => Halt()
    case (_, Halt()) => Halt()
    case (Emit(b, t1), Emit(c, t2)) => Emit((b,c), zip(t1, t2))
    case (Await(recv1), _) =>
      Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
    case (_, Await(recv2)) =>
      Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))
  }
  
def feed[A,B](oa: Option[A])(p: Process[A,B]): Process[A,B] =
  p match {
    case Halt() => p
    case Emit(h,t) => Emit(h, feed(oa)(t))
    case Await(recv) => recv(oa)
  }


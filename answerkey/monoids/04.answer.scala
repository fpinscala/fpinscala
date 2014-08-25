import fpinscala.testing._
import Prop._

def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  // Associativity
  forAll(for {
    x <- gen
    y <- gen
    z <- gen
  } yield (x, y, z))(p =>
    m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
  // Identity
  forAll(gen)((a: A) =>
    m.op(a, m.zero) == a && m.op(m.zero, a) == a)
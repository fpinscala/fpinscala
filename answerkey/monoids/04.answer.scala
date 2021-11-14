import fpinscala.testing.{Prop, Gen}
import Gen.`**`

def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  val associativity = Prop.forAll(gen ** gen ** gen) { case a ** b ** c =>
    m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
  }.tag("associativity")
  val identity = Prop.forAll(gen) { a =>
    m.combine(a, m.empty) == a && m.combine(m.empty, a) == a
  }.tag("identity")
  associativity && identity
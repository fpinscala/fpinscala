def endoMonoid[A]: Monoid[A => A] = new:
  def combine(f: A => A, g: A => A): A => A = f andThen g
  val empty: A => A = identity

def endoMonoidDual[A]: Monoid[A => A] = dual(endoMonoid)
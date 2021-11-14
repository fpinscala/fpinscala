def firstOptionMonoid[A]: Monoid[Option[A]] = new:
  def combine(x: Option[A], y: Option[A]) = x orElse y
  val empty = None

def lastOptionMonoid[A]: Monoid[Option[A]] = new:
  def combine(x: Option[A], y: Option[A]) = y orElse x
  val empty = None

def dual[A](m: Monoid[A]): Monoid[A] = new:
  def combine(x: A, y: A) = m.combine(y, x)
  val empty = m.empty

def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

def optionMonoid[A](f: (A, A) => A): Monoid[Option[A]] = new:
  def combine(x: Option[A], y: Option[A]) = x.map2(y)(f)
  val empty = None

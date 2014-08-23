// This is more efficient than `cons(a, constant(a))` since it's just
// one object referencing itself.
def constant[A](a: A): Stream[A] = {
  lazy val tail: Stream[A] = Cons(() => a, () => tail) 
  tail
}
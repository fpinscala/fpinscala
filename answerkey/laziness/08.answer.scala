// This is more efficient than `cons(a, constant(a))` since it's just
// one object referencing itself.
def constant[A](a: A): LazyList[A] = {
  lazy val tail: LazyList[A] = Cons(() => a, () => tail)
  tail
}
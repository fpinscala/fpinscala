It's because `foldRight`, `foldLeft`, and `foldMap` do not give us any way of constructing a value of the foldable type. In order to `map` over a structure, you need the ability to create a new structure (such as `Nil` and `Cons` in the case of a `List`). `Traverse` is able to extend `Functor` precisely because a traversal preserves the original structure. In short: `Foldable[F]` is essentially a homomorphism from `F` to `List`, but this is not always an isomorphism.

An example of a Foldable that is not a functor:

case class Iteration[A](a: A, f: A => A, n: Int) {
  def foldMap[B](g: A => B)(M: Monoid[B]): B = {
    def iterate(n: Int, b: B, c: A): B =
      if (n <= 0) b else iterate(n-1, g(c), f(a))
    iterate(n, M.zero, a)
  }
}
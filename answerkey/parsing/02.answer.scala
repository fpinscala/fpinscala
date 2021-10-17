`product` is associative. These two expressions are "roughly" equal:

  (a ** b) ** c
  a ** (b ** c)

The only difference is how the pairs are nested. The `(a ** b) ** c` parser returns an `((A,B), C)`, whereas the `a ** (b ** c)` returns an `(A, (B,C))`. We can define functions `unbiasL` and `unbiasR` to convert these nested tuples to flat 3-tuples:

  def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
  def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

With these, we can now state the associativity property:

  ((a ** b) ** c).map(unbiasL) == (a ** (b ** c)).map(unbiasR)

We'll sometimes just use `~=` when there is an obvious bijection between the two sides:

  (a ** b) ** c ~= a ** (b ** c)

`map` and `product` also have an interesting relationship--we can `map` either before or after taking the product of two parsers, without affecting the behavior:

  a.map(f) ** b.map(g) == (a ** b).map((a,b) => (f(a), g(b)))

For instance, if `a` and `b` were both `Parser[String]`, and `f` and `g` both computed the length of a string, it doesn't matter if we map over the result of `a` to compute its length, or whether we do that _after_ the product.

See chapter 12 for more discussion of these laws.

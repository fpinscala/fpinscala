Let's rewrite the following in terms of `flatMap`:

compose(compose(f, g), h) == compose(f, compose(g, h))

a => compose(f, g)(a).flatMap(h) == a => f(a).flatMap(compose(g, h))
a => ((b => flatMap(f(b))(g))(a)).flatMap(h) == a => f(a).flatMap(b => g(b).flatMap(h))

So far we have just expanded the definition of `compose`. Equals substituted for equals.
Let's simplify the left side a little:

a => f(a).flatMap(g).flatMap(h) == a => f(a).flatMap(b => g(b).flatMap(h))

Let's simplify again by eliminating the `a` argument and substituting a hypothetical value `x` for `f(a)`:

x.flatMap(g).flatMap(h) == x.flatMap(b => g(b).flatMap(h))

This now looks exactly like the monad law stated in terms of `flatMap`, just with different names:

x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

Q.E.D.
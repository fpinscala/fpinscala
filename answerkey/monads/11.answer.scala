Let's rewrite the following in terms of `flatMap`:

compose(compose(f, g), h) == compose(f, compose(g, h))

a => flatMap(compose(f, g)(a))(h) == a => flatMap(f(a))(compose(g, h))
a => flatMap((b => flatMap(f(b))(g))(a))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))

So far we have just expanded the definition of `compose`. Equals substituted for equals.
Let's simplify the left side a little:

a => flatMap(flatMap(f(a))(g))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))

Let's simplify again by eliminating the `a` argument and substituting a hypothetical value `x` for `f(a)`:

flatMap(flatMap(x)(g))(h) == flatMap(x)(b => flatMap(g(b))(h))

This now looks exactly like the monad law stated in terms of `flatMap`, just with different names:

flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))

Q.E.D.
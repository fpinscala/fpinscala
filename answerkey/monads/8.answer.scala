In our proof, we need to consider both cases of `Option`: `Some(v)` and `None`.
We start with the case where `x` is `None`, and then both sides of the equals sign are `None`:

None.flatMap(f).flatMap(g) == None.flatMap(a => f(a).flatMap(g))

Since `None.flatMap(f)` is `None` for all `f`, this is equivalent to:

None == None

But if `x` is `Some(v)`, then `Some(v).flatMap(f)` is `f(v)` for all `f` and `v`,
by the implementation of `flatMap` for `Option`:

Some(v).flatMap(f).flatMap(g) == Some(v).flatMap(a => f(a).flatMap(g))
f(v).flatMap(g)               == (a => f(a).flatMap(g))(v)
f(v).flatMap(g)               == f(v).flatMap(g)

Q.E.D.
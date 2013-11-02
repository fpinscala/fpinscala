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

Proving equivalence of the two `genOrder` implementations
is more challenging. We need to show equivalence of the
two ways of ways of threading the state actions to generate
random values. We omit the proof here, but a basic idea
is that both groupings generate the same sequence of `RNG`
values and use each `RNG` to produce the same portion of
the overall result.
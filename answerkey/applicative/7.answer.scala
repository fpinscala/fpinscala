We'll just work through left and right identity, but the basic idea for all these proofs is to substitute the definition of all functions, then use the monad laws to make simplifications to the applicative identities. 

Let's start with left and right identity:

    map2(unit(()), fa)((_,a) => a) == fa // Left identity
    map2(fa, unit(()))((a,_) => a) == fa // Right identity

We'll do left identity first. We expand definition of `map2`:

    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]
      flatMap(fa)(a => map(fb)(b => f(a,b)))

    flatMap(unit())(u => map(fa)(a => a)) == fa

We just substituted `unit(())` and `(_,a) => a` in for `f`. `map(fa)(a => a)` is just `fa` by the functor laws, giving us:

    flatMap(unit())(u => fa) == fa

Recall that `flatMap` can be rewritten using `compose`, by using `Unit` as the argument to the first function.

    compose(unit, u => fa)(()) == fa

And by the monad laws:

    compose(unit, f) == f

Therefore, `compose(unit, u => fa)` simplifies to `u => fa`. And `u` is just `Unit` here, and is ignored, so this is equivalent to `fa`:

    (u => fa)(()) == fa
    fa == fa

Right identity is symmetric; we just end up using the other identity for `compose`, that `compose(f, unit) == f`.

    flatMap(fa)(a => map(unit(()))(u => a)) == fa
    flatMap(fa)(a => unit(a)) == fa  // via functor laws
    compose(u => fa, unit)(()) == fa  
    (u => fa)(()) == fa
    fa == fa

Associativity and naturality are left as an exercise.
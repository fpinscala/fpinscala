To begin, the `apply` function can be implemented in terms of `map` and `flatMap`:

def apply[A,B](mf: M[A => B])(ma: M[A]): M[B] =
  flatMap(mf)(f => map(ma)(a => f(a)))

Now we take the applicative laws one by one.

-- Applicative identity law:
apply(unit(x => x))(v) == v
-- Substitute the implementation of `apply`:
flatMap(unit(x => x))(f => map(v)(a => f(a))) == v
-- Assuming the monad identity law:
(f => map(v)(a => f(a)))(x => x) == v
-- Simplify:
map(v)(a => a) == v
-- Which is just the functor identity law.

-- Applicative composition law:
   apply(apply(map(f)(a => b => c => a(b(c))))(g))(x)
== apply(f)(apply(g)(x))
-- Substitute defition of `apply`:
   flatMap(apply(map(f)(a => b => c => a(b(c))))(g))(h => map(x)(y => h(y)))
== flatMap(f)(h => map(apply(g)(x))(y => h(y)))
-- Substitute definition of `apply`:
   flatMap(flatMap(map(f)(a => b => c => a(b(c))))(h => map(g)(y => h(y))))(h => map(x)(y => h(y)))
== flatMap(f)(h => map(flatMap(g)(i => map(x)(y => i(y))))(y => h(y)))
-- Assume monad associative law:
   flatMap(map(f)(a => b => c => a(b(c))))(a => flatMap(map(g)(y => a(y)))(h => map(x)(y => h(y))))
== flatMap(f)(h => map(flatMap(g)(i => map(x)(y => i(y))))(y => h(y)))
-- Substitute definition of `map`:
   flatMap(flatMap(f)(a => unit(b => c => a(b(c)))))(a => flatMap(flatMap(g)(y => unit(a(y))))(h => map(x)(y => h(y))))
== flatMap(f)(h => flatMap(flatMap(g)(i => map(x)(y => i(y))))(y => unit(h(y))))
-- Assume monad associative law:
   flatMap(f)(d => flatMap(unit(b => c => d(b(c))))(a => flatMap(g)(e => flatMap(unit(a(e)))(h => map(x)(y => h(y))))))
== flatMap(f)(h => flatMap(g)(j => flatMap(map(x)(y => j(y)))(y => unit(h(y)))))
-- Assume monad identity law:
   flatMap(f)(d => flatMap(g)(e => map(x)(y => d(e(y)))))
== flatMap(f)(h => flatMap(g)(j => flatMap(map(x)(y => j(y)))(y => unit(h(y)))))
-- Substitute definition of `map`:
   flatMap(f)(d => flatMap(g)(e => flatMap(x)(y => unit(d(e(y))))))
== flatMap(f)(h => flatMap(g)(j => flatMap(flatMap(x)(y => unit(j(y))))(y => unit(h(y)))))
-- Assume monad associative law:
   flatMap(f)(d => flatMap(g)(e => flatMap(x)(y => unit(d(e(y))))))
== flatMap(f)(h => flatMap(g)(j => flatMap(x)(k => flatMap(unit(j(k)))(y => unit(h(y))))))
-- Assume monad identity law:
   flatMap(f)(d => flatMap(g)(e => flatMap(x)(y => unit(d(e(y))))))
== flatMap(f)(h => flatMap(g)(j => flatMap(x)(k => unit(h(j(k))))))


-- Applicative homomorphism law:
apply(unit(f))(unit(x)) == unit(f(x))
-- Substitute definition of `apply`:
flatMap(unit(f))(k => map(unit(x))(a => k(a))) == unit(f(x))
-- Assume monad identity law:
map(unit(x))(a => f(a)) == unit(f(x))
-- Definition of map:
flatMap(unit(x))(a => unit(f(a))) == unit(f(x))
-- Assume monad identity law:
unit(f(x)) == unit(f(x))

-- Applicative interchange law:
apply(u)(unit(y)) == apply(unit(_(y)))(u)
-- Definition of `apply`:
flatMap(u)(k => map(unit(y))(a => k(a))) == flatMap(unit(_(y)))(k => map(u)(a => k(a)))
-- Definition of `map`:
flatMap(u)(k => flatMap(unit(y))(a => unit(k(a)))) == flatMap(unit(_(y)))(k => flatMap(u)(a => unit(k(a))))
-- Assume monad identity law:
flatMap(u)(k => unit(k(y))) == flatMap(u)(a => unit(a(y)))

Q.E.D.

Notice that the only law that we needed associativity for was composition. All the others are some variant of the monad identity law.
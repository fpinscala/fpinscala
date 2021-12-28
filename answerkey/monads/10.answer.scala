// We simply substitute the definition of `compose` in terms of `flatMap`.

compose(f, unit)(v) == f(v)          // for all functions f and values v
(a => f(a).flatMap(unit))(v) == f(v) // Expand `compose`
f(v).flatMap(unit) == f(v)           // Simplify function application
x.flatMap(unit) == x                 // Abstract out `f(v)`

compose(unit, f)(x) == f(x)
unit(x).flatMap(f) == f(x)           // Expand `compose`

Q.E.D.
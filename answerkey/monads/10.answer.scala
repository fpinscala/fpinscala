// We simply substitute the definition of `compose` in terms of `flatMap`.

compose(f, unit)(v) == f(v)           // for all functions f and values v
(a => flatMap(f(a))(unit))(v) == f(v) // Expand `compose`
flatMap(f(v))(unit) == f(v)           // Simplify function application
flatMap(x)(unit) == x                 // Abstract out `f(v)`

compose(unit, f)(x) == f(x)
flatMap(unit(x))(f) == f(x) // Expand `compose`

Q.E.D.
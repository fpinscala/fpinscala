// This is simply substituting the definition of `compose` in terms of `flatMap`.
compose(f, unit) == f         // for all functions f
a => flatMap(f(a))(unit) == f // for all functions f
flatMap(x)(unit) == x         // for all values x

compose(unit, f) == f       // for all functions f
flatMap(unit(x))(f) == f(x) // for all functions f and all values x
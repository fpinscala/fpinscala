/* 
When we implemented `map` in terms of `map2` and `unit`, we chose, rather arbitrarily, to put the dummy value on the right. We expect that it would mean the same to put this dummy value on the left:

map2(x, unit(()))(_._1) == map2(unit(()), x)(_._2)

Not very exciting, but somewhat related, `map2` seems like it ought to be associative. That is:

product(product(a,b),c) ~ product(a, product(b,c))

Of course, the tuples will be grouped differently on each side of the equation. Associativity is an extremely common property, and it's very desirable. Without it, we have to be aware of and make decisions about how to factor our expressions. 
*/
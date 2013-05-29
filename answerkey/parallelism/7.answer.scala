/* 
See this post by Derek Elkins on the Haskell mailing list, giving the derivation:
http://www.haskell.org/pipermail/haskell-cafe/2010-January/071636.html

Brief note on translating this syntax: In Haskell, `f . g` is function composition, `f compose g` or `(x => f(g(x)))` in Scala. And `fmap` is equivalent to our function `map`, but with the arguments flipped, so `fmap f x == map(x)(f)`.
*/
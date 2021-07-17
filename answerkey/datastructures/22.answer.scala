/*
To match on multiple values, we can put the values into a pair and match on the pair, as shown next, and the same
syntax extends to matching on N values (see sidebar "Pairs and tuples in Scala" for more about pair and tuple
objects). You can also (somewhat less conveniently, but a bit more efficiently) nest pattern matches: on the
right hand side of the `=>`, simply begin another `match` expression. The inner `match` will have access to all the
variables introduced in the outer `match`.

The discussion about stack usage from the explanation of `map` also applies here.
*/
def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))

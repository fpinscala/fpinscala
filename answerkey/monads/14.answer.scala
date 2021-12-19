We can look at the associative law in terms of `flatMap` from another perspective. It says that `x.flatMap(f).flatMap(g)` is equal to `x.flatMap(a => f(a).flatMap(g))` _for all_ choices of `f` and `g`. So let's pick a particular `f` and `g` that's easy to think about. We can just pick the identity function:

x.flatMap(z => z).flatMap(z => z) == x.flatMap(a => a.flatMap(z => z))

And of course, flatMapping with the identify function is `join`! The associative law can now be stated as:

x.join.join == x.flatMap(_.join)

And we know that `flatMap` is "map, then join," so let's eliminate this last call to `flatMap`:

x.join.join == x.map(_.join).join

The identity laws in terms of `join` are:

x.map(unit).join == x
unit(x).join == x

This follows from the definition of `join` and the identity laws in terms of `flatMap`:

x.flatMap(unit) == x
unit(x).flatMap(f) == f(x)

For the second law, we simply substitute the identity function for `f`, which gives us `join`.

Let's make a fast-and-loose proof for this version of the associative law using the `List` monad as an example. Of course, `join` in the `List` monad is just _list concatenation_:

scala> listMonad.join(List(List(1, 2), List(3, 4)))
res0: List[Int] = List(1, 2, 3, 4)

Now let's say we have some lists, nested to a depth of three:

val ns: List[List[List[Int]]] =
  List(List(List(1,2), List(3,4)),
       List(List(), List(5)))

If we `join` this list, the outer lists get concatenated and we have a list of lists two levels deep:

scala> ns.flatten
res1: List[List[Int]] = List(List(1, 2), List(3, 4), List(), List(5))

If we instead _map_ `join` over it, we get a different nesting but again two levels deep. This flattens the _inner_ lists.

scala> ns.map(_.join)
res2: List[List[Int]] = List(List(1, 2, 3, 4), List(5))

And then joining `res1` should be the same as joining `res2`:

scala> res1.join == res2.join
res3: Boolean = true

So all that the associative law is saying for the `List` monad is that concatenating the outer lists and then the inner ones (`join(join(ns))`) is the same as first concatenating the inner lists and then the outer ones (`join(ns.map(join))`).
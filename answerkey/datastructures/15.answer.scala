/*
Since `append` takes time proportional to its first argument, and this first argument never grows because of the
right-associativity of `foldRight`, this function is linear in the total length of all lists. You may want to try
tracing the execution of the implementation on paper to convince yourself that this works.

Note that we're simply referencing the `append` function, without writing something like `(x,y) => append(x,y)`
or `append(_,_)`. In Scala there is a rather arbitrary distinction between functions defined as _methods_, which are
introduced with the `def` keyword, and function values, which are the first-class objects we can pass to other
functions, put in collections, and so on. This is a case where Scala lets us pretend the distinction doesn't exist.
In other cases, you'll be forced to write `append _` (to convert a `def` to a function value)
or even `(x: List[A], y: List[A]) => append(x,y)` if the function is polymorphic and the type arguments aren't known.
*/
def concat[A](l: List[List[A]]): List[A] =
  foldRight(l, Nil:List[A], append)


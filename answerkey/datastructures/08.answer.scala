/*
We get back the original list! Why is that? As we mentioned earlier, one way of thinking about what `foldRight` "does"
is it replaces the `Nil` constructor of the list with the `z` argument, and it replaces the `Cons` constructor with
the given function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`, then we get back the input list.

foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
Cons(1, Cons(2, Cons(3, Nil)))
*/


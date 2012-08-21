/* 
Notice that we traverse all the way to the end of the list (pushing frames onto the call stack as we go) before we can begin collapsing it.

foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
1 + foldRight(Cons(2, Cons(3, Nil)), 0)(_ + _)
1 + (2 + foldRight(Cons(3, Nil), 0)(_ + _))
1 + (2 + (3 + (foldRight(Nil, 0)(_ + _))))
1 + (2 + (3 + (0)))
6
*/
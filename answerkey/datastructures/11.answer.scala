/* 
It's common practice to annotate functions you expect to be tail-recursive with the `tailrec` annotation. If the function is not tail-recursive, it will yield a compile error, rather than silently compiling the code and resulting in greater stack space usage at runtime. 

Aside: we usually use one-letter variable names when everything there is to say about a value is implied by its type. Since functions are usually quite short in FP, many functional programmers feel this makes the code easier to read, since it makes the structure of the code easier to see. 
*/
@annotation.tailrec
def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match { 
  case Nil => z
  case Cons(h,t) => foldLeft(t, f(z,h))(f)
}
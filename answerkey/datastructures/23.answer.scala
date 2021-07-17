/*
This function is usually called `zipWith`. The discussion about stack usage from the explanation of `map` also
applies here. By putting the `f` in the second argument list, Scala can infer its type from the previous argument list.
*/
def zipWith[A,B,C](a: List[A], b: List[B], f: (A,B) => C): List[C] = (a,b) match
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))

/*
Tail recursive implementation of `zipWith`. 
*/
def zipWith_1[A,B,C](a: List[A], b: List[B], f: (A,B) => C): List[C] =
  @annotation.tailrec
  def loop(a: List[A], b: List[B], acc: List[C]): List[C] =
    (a, b) match
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), acc))
  reverse(loop(a, b, Nil))

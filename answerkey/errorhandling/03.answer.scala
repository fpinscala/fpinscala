// a bit later in the chapter we'll learn nicer syntax for 
// writing functions like this
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(aa => b.map(bb => f(aa, bb)))
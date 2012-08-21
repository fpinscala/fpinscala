/* 
The for-comprehension syntax is somewhat clearer. Here are both versions: 
*/
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a flatMap (aa =>
  b map     (bb =>
  f(aa, bb)))

def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
  for { 
    a1 <- a 
    b1 <- b 
  } yield f(a1,b1)
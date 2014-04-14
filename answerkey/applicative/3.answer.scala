/* 
The pattern is simple. We just curry the function 
we want to lift, pass the result to `unit`, and then `apply` 
as many times as there are arguments. 
Each call to `apply` is a partial application of the function
*/
def map3[A,B,C,D](fa: F[A],
                  fb: F[B],
                  fc: F[C])(f: (A, B, C) => D): F[D] =
  apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

def map4[A,B,C,D,E](fa: F[A],
                    fb: F[B],
                    fc: F[C],
                    fd: F[D])(f: (A, B, C, D) => E): F[E]
  apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
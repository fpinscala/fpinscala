/* 
We curry the function we want to lift, pass the result to `unit`, and then `apply` 
as many times as there are arguments. 
Each call to `apply` is a partial application of the function
*/
extension [A](fa: F[A])
  def map[B](f: A => B): F[B] =
    apply(f.curried)(fa)
  
  def map2[B, C](fb: F[B])(f: (A, B) => B): F[C] =
    apply(fa.map(f.curried))(fb)

  def map3[B, C, D](
    fb: F[B],
    fc: F[C]
  )(f: (A, B, C) => D): F[D] =
    apply(fa.map2(fb)((a, b) => f(a, b, _)))(fc)

  def map4[B, C, D, E](
    fb: F[B],
    fc: F[C],
    fd: F[D]
  )(f: (A, B, C, D) => E): F[E] =
    apply(fa.map3(fb, fc)((a, b, c) => f(a, b, c, _)))(fc)

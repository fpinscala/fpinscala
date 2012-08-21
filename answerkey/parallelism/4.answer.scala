def asyncF[A,B](f: A => B): A => Par[B] = 
  a => fork(unit(f(a)))
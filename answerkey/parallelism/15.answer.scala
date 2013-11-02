def flatMap[B](f: A => Par[B]): Par[B] = 
  Par.flatMap(p)(f)

def map[B](f: A => B): Par[B] = 
  Par.map(p)(f)

def map2[B,C](p2: Par[B])(f: (A,B) => C): Par[C] =
  Par.map2(p,p2)(f)

def zip[B](p2: Par[B]): Par[(A,B)] = 
  p.map2(p2)((_,_))
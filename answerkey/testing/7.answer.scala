def map[B](f: A => B): Gen[B] = 
  Gen(sample.map(f), exhaustive.map(_.map(f)))

def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] = 
  Gen(sample.map2(g.sample)(f), 
      map2Stream(exhaustive,g.exhaustive)(map2Option(_,_)(f)))
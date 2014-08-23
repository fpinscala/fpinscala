def sum2: Process[Double,Double] =
  loop(0.0)((d:Double, acc) => (acc+d,acc+d))

def count3[I]: Process[I,Int] =
  loop(0)((_:I,n) => (n+1,n+1))


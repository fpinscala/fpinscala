def variance(xs: Seq[Double]): Option[Double] = 
  mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
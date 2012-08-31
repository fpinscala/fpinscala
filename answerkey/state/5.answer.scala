def positiveMax(n: Int): Rand[Int] =
  map(positiveInt)(_ / (Int.MaxValue / n))
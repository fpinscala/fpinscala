val _nextDouble: Rand[Double] =
  map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))
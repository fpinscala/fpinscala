val _double: Rand[Double] =
  map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))
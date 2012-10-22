def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val (i, r1) = rng.nextInt
  val (d, r2) = double(r1)
  ((i, d), r2)
}

def doubleInt(rng: RNG): ((Double, Int), RNG) = {
  val ((i, d), r) = intDouble(rng)
  ((d, i), r)
}

def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  val (d1, r1) = double(rng)
  val (d2, r2) = double(r1)
  val (d3, r3) = double(r2)
  ((d1, d2, d3), r3)
}

// There is something terribly repetitive about passing the RNG along
// every time. What could we do to eliminate some of this duplication
// of effort?
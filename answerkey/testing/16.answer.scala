/* A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
 * computation for each element of the input list summed to produce the final 
 * result. This is not the most compelling example, but it provides at least some 
 * variation in structure to use for testing. 
 */
val gpy2: Gen[Par[Int]] = 
  choose(-100, 100).listOfN(choose(0, 20)).map(ys =>
    ys.foldLeft(Par.unit(0))((p, y) =>
      Par.fork(p.map2(Par.unit(y))(_ + _))))
These methods are defined in the `Gen` class:

  def flatMap[B](f: A => Gen[B]): Gen[B] = 
    Gen(sample.flatMap(a => f(a).sample),
        exhaustive.flatMap {
          case None => unbounded 
          case Some(a) => f(a).exhaustive
        })
  
  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] = 
    Gen.listOfN(size, this)
  
  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] = 
    size flatMap (n => this.listOfN(n))

These methods are defined in the `Gen` companion object: 

  /* Basic idea is add 1 to the result of `choose` if it is of the wrong
   * parity, but we require some special handling to deal with the maximum 
   * integer in the range.
   */
  def even(start: Int, stopExclusive: Int): Gen[Int] = 
    choose(start, if (stopExclusive%2 == 0) stopExclusive - 1 else stopExclusive).
    map (n => if (n%2 != 0) n+1 else n)
  
  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive%2 != 0) stopExclusive - 1 else stopExclusive).
    map (n => if (n%2 == 0) n+1 else n)
     
  def sameParity(from: Int, to: Int): Gen[(Int,Int)] = for {
    i <- choose(from,to)
    j <- if (i%2 == 0) even(from,to) else odd(from,to)
  } yield (i,j)
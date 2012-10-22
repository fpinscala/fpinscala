def genStringIntFn(g: Gen[Int]): Gen[String => Int] = 
  g map (i => (s => i))
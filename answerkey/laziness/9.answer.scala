val fibs = {
  def go(f0: Int, f1: Int): Stream[Int] = 
    cons(f0, go(f1, f0+f1))
  go(0, 1)
}
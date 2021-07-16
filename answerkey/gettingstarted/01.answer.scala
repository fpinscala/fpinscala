// 0 and 1 are the first two numbers in the sequence,
// so we start the accumulators with those.
// At every iteration, we add the two numbers to get the next one.
def fib(n: Int): Int =
  @annotation.tailrec
  def loop(n: Int, prev: Int, cur: Int): Int =
    if n == 0 then prev
    else loop(n - 1, cur, prev + cur)
  loop(n, 0, 1)

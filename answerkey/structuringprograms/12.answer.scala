def fibRec(n: Int): Int = if (n < 2) n else fib(n - 1) + fib(n - 2)

// The recursive definition is very slow, and uses binary recursion (calls itself twice).
// Here is one example of a tail-recursive definition:
def fib(n: Int): Int = {
  def loop(n: Int, x: Int, y: Int): Int =
    if (n == 0) x else loop(n - 1, y, x + y)
  loop(n, 0, 1)
}

// 0 and 1 are the first two numbers in the sequence, so we start the accumulators with those.
// At every iteration, we add the two numbers to get the next one.
The key insight is that generating a function involves transforming a generator. These generator transformers can be expressed using a type like this:

  trait Cogen[-A] {
    def sample[B](a: A, s: State[RNG,B]): State[RNG,B]
    def exhaustive[B](a: A, s: Stream[Option[B]]): Stream[Option[B]]
  }

Given a `Cogen[A]` and a `Gen[B]`, we can create a `Gen[A => B]`, by transforming the `sample` and `exhaustive` members using the `Cogen` (you are encoraged to try writing this function):

  def fn[A,B](in: Cogen[A])(out: Gen[B]): Gen[A => B]

The only real requirement of a `Cogen[A]` is that it _use the information_ of the input `A` to define the transformation of the random and exhaustive streams. There are many ways to do this - it could pattern match on the `A` and use this to set the seed of the random number generator and determine a permutation of the exhaustive stream.
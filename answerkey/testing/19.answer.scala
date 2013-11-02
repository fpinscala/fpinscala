The key insight is that generating a function involves transforming a generator. These generator transformers can be expressed using a type like this:

  trait Cogen[-A] {
    def sample[B](a: A, s: State[RNG,B]): State[RNG,B]
  }

Given a `Cogen[A]` and a `Gen[B]`, we can create a `Gen[A => B]`, by transforming the `sample` members using the `Cogen` (you are encouraged to try writing this function):

  def fn[A,B](in: Cogen[A])(out: Gen[B]): Gen[A => B]

The only real requirement of a `Cogen[A]` is that it _use the information_ of the input `A` to define the transformation of the generator. There are many ways to do this--for instance it could pattern match on the `A` and use this to set the seed of the random number generator.
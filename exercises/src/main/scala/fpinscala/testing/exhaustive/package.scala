package fpinscala.testing

package object exhaustive {
  type Rand[A] = State[RNG, A]

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

}

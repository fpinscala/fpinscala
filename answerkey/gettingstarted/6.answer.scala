object Predicates {
  val _divisibleBy3And5: Pred[Int] = n => divisibleBy(3)(n) && divisibleBy(5)(n)
  val _divisibleBy3Or5: Pred[Int] = n => divisibleBy(3)(n) || divisibleBy(5)(n)

  def lift[A](f: (Boolean, Boolean) => Boolean, g: Pred[A], h: Pred[A]): Pred[A] = 
    a => f(g(a), h(a))

  val divisibleBy3And5: Pred[Int] = lift(_ && _, divisibleBy(3), divisibleBy(5))
  val divisibleBy3Or5: Pred[Int]  = lift(_ || _, divisibleBy(3), divisibleBy(5))
}
/*
Calling `divisibleBy(0)` results in an error. But we get different
results for these two expressions:

lift(_ || _, divisibleBy(2), divisibleBy(0))
(n: Int) => divisibleBy(2)(n) || divisibleBy(0)(n)

Try them with different inputs. Why do you think one of them fails with
an error for even numbers and the other one just returns `true` without
failing? Do you think this has any implications for referential
transparency? Make a note of your thoughts and revisit this question
after reading the chapter on "strictness and laziness".
*/
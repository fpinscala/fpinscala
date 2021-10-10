Here are a few properties:

* The sum of the empty list is 0 -- `sum(Nil) == 0`.
* The sum of a list whose elements are all equal to `x` is the list's size multiplied by `x` -- `sum(List.fill(n)(x)) == n * x`.
* For any list `xs`, its sum is equal to the sum of `xs.reverse` -- `sum(xs) == sum(xs.reverse)`. This follows from the fact that addition is commutative.
* For any list `xs`, partitioning it in to two lists, summing each partitioning, and then adding the sums yields same result as summing `xs`. This follows from the fact that addition is associative.
* The sum of the list with elements 1, 2, 3...`n` is `n*(n+1)/2`.
